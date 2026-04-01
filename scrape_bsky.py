import os
import csv
import random
from atproto import Client
from atproto_client.exceptions import (
    UnauthorizedError,
    NetworkError,
    RequestException,
    BadRequestError,
    InvokeTimeoutError,
    LoginRequiredError,
)
from atproto_core.exceptions import AtProtocolError
from sentiment_analysis import (
    analyze_and_attach,
    SentimentInitError,
    SentimentInferenceError,
)
import time

DISCOVER_URI = "at://did:plc:z72i7hdynmk6r22z27h6tvur/app.bsky.feed.generator/whats-hot"
TARGET = 50
PAGE_LIMIT = 70

LOGINS_CSV = "Logins.csv"
OUTPUT_CSV = "bsky_sentiment_log.csv"


def load_logins(csv_path: str):
    """
    Load (handle, password) pairs from a CSV file where:
      - handle is in column 0
      - password is in column 2
    Skips empty rows and header row if present.
    """
    logins = []
    with open(csv_path, newline="", encoding="utf-8") as f:
        reader = csv.reader(f)
        for row in reader:
            if not row:
                continue
            if row[0].strip().lower() in ("handle", "username"):
                continue

            handle = row[0].strip()
            password = row[2].strip() if len(row) > 2 else ""

            if handle and password:
                logins.append((handle, password))
    return logins


def get_next_sample_number(output_csv: str) -> int:
    """
    Look at the existing OUTPUT_CSV (if any) and return the next sample_number.
    This is computed ONCE per run, and shared by all bots in this run.
    """
    if not os.path.isfile(output_csv):
        return 1

    with open(output_csv, "r", encoding="utf-8") as f:
        reader = csv.DictReader(f)
        rows = list(reader)
        if not rows:
            return 1
        last = rows[-1]
        try:
            return int(last.get("sample_number", 0)) + 1
        except ValueError:
            return 1



def login_bot(bot_handle: str, bot_password: str) -> Client | None:
    """
    Log in a bot and return the Client, or None if login fails.
    Only catches atproto-specific exceptions, not generic Exception.
    """
    client = Client()
    try:
        profile = client.login(bot_handle, bot_password)
        print(f"[{bot_handle}] Logged in as {profile.handle}")
        return client

    except UnauthorizedError as e:
        print(f"[{bot_handle}] Unauthorized (bad handle/app password?): {e}")
        return None

    except LoginRequiredError as e:
        print(f"[{bot_handle}] LoginRequiredError while logging in: {e}")
        return None

    except (NetworkError, InvokeTimeoutError, RequestException) as e:
        print(f"[{bot_handle}] Network/request error during login: {e}")
        return None

    except (BadRequestError, AtProtocolError) as e:
        print(f"[{bot_handle}] Protocol error during login: {e}")
        return None



def scrape_feed_for_bot(client: Client, bot_handle: str) -> dict[str, list]:
    """
    Scrape TARGET English posts from the Discover feed for this bot.
    Retries each get_feed() call up to 3 times with exponential backoff
    on network/protocol errors.
    """
    feed_data = {
        "text": [],
        "CID": [],
        "handle": [], 
        "URI": [],
    }

    cursor = None
    max_retries = 3
    base_delay = 1.0 

    while len(feed_data["text"]) < TARGET:
        attempt = 0
        res = None

        while attempt < max_retries:
            try:
                res = client.app.bsky.feed.get_feed(
                    {"feed": DISCOVER_URI, "limit": PAGE_LIMIT, "cursor": cursor}
                )
                break

            except (NetworkError, InvokeTimeoutError, RequestException, BadRequestError, AtProtocolError) as e:
                attempt += 1
                if attempt >= max_retries:
                    print(
                        f"[{bot_handle}] Giving up on fetching feed page after "
                        f"{max_retries} attempts: {e}"
                    )
                    # return whatever we have so far; caller decides what to do
                    print(f"[{bot_handle}] Collected {len(feed_data['text'])} English posts (partial).")
                    return feed_data

                sleep_time = base_delay * (2 ** (attempt - 1))
                print(
                    f"[{bot_handle}] Feed fetch failed (attempt {attempt}/{max_retries}). "
                    f"Retrying in {sleep_time:.1f}s: {e}"
                )
                time.sleep(sleep_time)

        if res is None or not res.feed:
            break

        for f in res.feed:
            post = getattr(f, "post", None)
            rec = getattr(post, "record", None)

            text = getattr(rec, "text", None)
            langs = getattr(rec, "langs", [])  # language tags
            cid = getattr(post, "cid", None)
            author_handle = getattr(post.author, "handle", None)
            uri = getattr(post, "uri", None)

            # language + text filters
            if not langs or "en" not in langs:
                continue
            if not text:
                continue

            feed_data["text"].append(text)
            feed_data["CID"].append(cid)
            feed_data["handle"].append(author_handle)
            feed_data["URI"].append(uri)

            if len(feed_data["text"]) >= TARGET:
                break

        cursor = res.cursor
        if not cursor:
            # restart from top of Discover until we hit target
            cursor = None
            continue

    print(f"[{bot_handle}] Collected {len(feed_data['text'])} English posts")
    return feed_data


def analyze_sentiment(feed_data):
    """
    Run RoBERTa sentiment analysis on the feed texts.
    Returns combined_results list.
    """
    texts = feed_data["text"]
    combined_results = analyze_and_attach(texts)
    return combined_results


def group_by_sentiment(feed_data, combined_results, bot_handle: str):
    """
    Group posts into positive/neutral/negative buckets.
    Returns sentiment_groups dict.
    """
    sentiment_groups = {
        "positive": [],
        "neutral": [],
        "negative": [],
    }

    for idx, result in enumerate(combined_results):
        label = result["label"].lower()  # "positive", "neutral", "negative"
        if label not in sentiment_groups:
            continue

        entry = {
            "index": idx,
            "text": feed_data["text"][idx],
            "handle": feed_data["handle"][idx],
            "CID": feed_data["CID"][idx],
            "URI": feed_data["URI"][idx],
            "sentiment": label,
            "confidence": float(result["score"]),
        }

        sentiment_groups[label].append(entry)

    positive_posts = sentiment_groups["positive"]
    neutral_posts = sentiment_groups["neutral"]
    negative_posts = sentiment_groups["negative"]

    print(f"\n[{bot_handle}] Bucket sizes for this sample:")
    print(f"  positive: {len(positive_posts)}")
    print(f"  neutral:  {len(neutral_posts)}")
    print(f"  negative: {len(negative_posts)}")

    return sentiment_groups


def pretty_print_results(bot_handle: str, combined_results):
    """
    Optional: print each post with its sentiment and confidence.
    """
    print("\n==========================")
    print(f" SENTIMENT ANALYSIS RESULTS for {bot_handle}")
    print("==========================\n")

    for idx, item in enumerate(combined_results, start=1):
        print(f"Post #{idx}:")
        print(f"Text: {item['text']}")
        print(f"Sentiment: {item['label']}  (confidence={item['score']:.4f})")
        print("-" * 50)


def decide_likes(sentiment_groups, bot_handle: str):
    """
    Decide which posts are ELIGIBLE to be liked *based on bot handle prefix*.

    Behavior:
      - handles starting with "c": like random posts from ALL sentiments
      - handles starting with "n": like random NEGATIVE posts
      - handles starting with "p": like random POSITIVE posts
      - anything else: default to POSITIVE posts

    We still compute sentiment for all posts; this function only decides
    which subset is used as the candidate pool for likes.

    Returns:
      (candidate_posts_list, desired_likes_int)
    where each candidate post is a dict with:
      index, text, handle, CID, URI, sentiment, confidence
    """
    positive_posts = sentiment_groups["positive"]
    neutral_posts = sentiment_groups["neutral"]
    negative_posts = sentiment_groups["negative"]

    stripped_handle = bot_handle.lstrip("@")
    first_char = stripped_handle[0].lower() if stripped_handle else ""

    if first_char == "c":
        candidate_posts = positive_posts + neutral_posts + negative_posts
        strategy_desc = "random from ALL sentiments"

    elif first_char == "n":
        candidate_posts = negative_posts
        strategy_desc = "NEGATIVE-only"

    elif first_char == "p":
        candidate_posts = positive_posts
        strategy_desc = "POSITIVE-only"

    else:
        candidate_posts = positive_posts
        strategy_desc = "POSITIVE-only (default)"

    if not candidate_posts:
        print(
            f"\n[{bot_handle}] No candidate posts available for strategy "
            f"{strategy_desc}; will not like any posts."
        )
        return [], 0

    desired_likes = min(5, len(candidate_posts))

    print(
        f"\n[{bot_handle}] Strategy: {strategy_desc}. "
        f"Will attempt to like {desired_likes} post(s) "
        f"out of {len(candidate_posts)} available."
    )
    return candidate_posts, desired_likes


def append_results_to_csv(
    feed_data,
    combined_results,
    bot_handle: str,
    sample_number: int,
    liked_indices,
):

    fieldnames = [
        "sample_number",
        "bot_handle",
        "text",
        "handle",
        "CID",
        "URI",
        "sentiment",
        "confidence",
        "Liked?",
    ]

    file_exists = os.path.isfile(OUTPUT_CSV)

    with open(OUTPUT_CSV, mode="a", newline="", encoding="utf-8") as csvfile:
        writer = csv.DictWriter(csvfile, fieldnames=fieldnames)

        # write header if file is new
        if not file_exists:
            writer.writeheader()

        for i, item in enumerate(combined_results):
            writer.writerow({
                "sample_number": sample_number,
                "bot_handle": bot_handle,
                "text": feed_data["text"][i],
                "handle": feed_data["handle"][i],
                "CID": feed_data["CID"][i],
                "URI": feed_data["URI"][i],
                "sentiment": item["label"],
                "confidence": float(item["score"]),
                "Liked?": i in liked_indices,   # true if the post was liked
            })

    print(
        f"\n[{bot_handle}] Appended {len(combined_results)} rows to "
        f"{OUTPUT_CSV} with sample_number={sample_number}"
    )


def perform_likes(
    client: Client,
    bot_handle: str,
    candidate_posts: list[dict],
    desired_likes: int,
) -> set[int]:

    liked_indices: set[int] = set()

    if not candidate_posts or desired_likes <= 0:
        print(f"\n[{bot_handle}] No posts selected to like.")
        return liked_indices

    print(f"\n[{bot_handle}] Liking posts now (target successful likes: {desired_likes})")

    max_retries_per_call = 3
    base_delay = 1.0 


    all_candidate_ids = list(range(len(candidate_posts)))
    untried_ids = set(all_candidate_ids)

    max_global_attempts = desired_likes * max(3, len(all_candidate_ids))
    global_attempts = 0

    while len(liked_indices) < desired_likes and global_attempts < max_global_attempts:
        global_attempts += 1

        if untried_ids:
            local_id = random.choice(tuple(untried_ids))
            untried_ids.remove(local_id)
        else:

            retry_pool = [
                i for i in all_candidate_ids
                if candidate_posts[i]["index"] not in liked_indices
            ]
            if not retry_pool:
                break
            local_id = random.choice(retry_pool)

        post = candidate_posts[local_id]
        orig_idx = post["index"] 

        if orig_idx in liked_indices:
            continue

        uri = post["URI"]
        cid = post["CID"]

        attempt = 0
        success = False

        while attempt < max_retries_per_call:
            attempt += 1
            try:
                client.like(uri, cid)
                success = True
                print(
                    f"- [{bot_handle}] Liked post index {post['index']} | "
                    f"{post['sentiment']} ({post['confidence']:.3f})"
                )
                print(f"  URI:   {uri}")
                print(
                    f"  Text:  {post['text'][:120]}"
                    f"{'...' if len(post['text']) > 120 else ''}"
                )
                break 

            except (UnauthorizedError, LoginRequiredError) as e:
                print(
                    f"! [{bot_handle}] Auth error when liking post index "
                    f"{post['index']} ({uri}): {e}"
                )
                return liked_indices

            except (NetworkError, InvokeTimeoutError, RequestException, BadRequestError, AtProtocolError) as e:
                if attempt >= max_retries_per_call:
                    print(
                        f"! [{bot_handle}] Giving up on liking post index "
                        f"{post['index']} ({uri}) after {max_retries_per_call} attempts: {e}"
                    )
                    break

                sleep_time = base_delay * (2 ** (attempt - 1))
                print(
                    f"! [{bot_handle}] Error liking post index {post['index']} "
                    f"(attempt {attempt}/{max_retries_per_call}). Retrying in "
                    f"{sleep_time:.1f}s: {e}"
                )
                time.sleep(sleep_time)

        if success:
            liked_indices.add(orig_idx)

    if len(liked_indices) < desired_likes:
        print(
            f"\n[{bot_handle}] Could only successfully like "
            f"{len(liked_indices)}/{desired_likes} posts despite retries."
        )
    else:
        print(
            f"\n[{bot_handle}] Successfully liked {len(liked_indices)}/"
            f"{desired_likes} posts."
        )

    return liked_indices

def run_for_bot(bot_handle: str, bot_password: str, sample_number: int):
    print(f"\n==============================")
    print(f"Running session for {bot_handle} (sample {sample_number})")
    print(f"==============================")

    client = login_bot(bot_handle, bot_password)
    if client is None:
        return

    feed_data = scrape_feed_for_bot(client, bot_handle)

    if not feed_data["text"]:
        print(f"[{bot_handle}] No posts collected, skipping sentiment.")
        return


    max_sentiment_retries = 3
    base_delay = 1.0  # seconds
    combined_results = []

    for idx, text in enumerate(feed_data["text"]):
        attempt = 0
        while attempt < max_sentiment_retries:
            attempt += 1
            try:
                result = analyze_and_attach([text])[0]  # single-text inference
                combined_results.append(result)
                break
            except (SentimentInitError, SentimentInferenceError) as e:
                if attempt >= max_sentiment_retries:
                    print(
                        f"[{bot_handle}] Sentiment failed for post #{idx} "
                        f"after {max_sentiment_retries} attempts → assigning unknown"
                    )
                    combined_results.append({
                        "text": text,
                        "label": "unknown",
                        "score": 0.0
                    })
                else:
                    sleep_time = base_delay * (2 ** (attempt - 1))
                    print(
                        f"[{bot_handle}] Sentiment retry {attempt}/{max_sentiment_retries} "
                        f"for post #{idx}: {e} → sleep {sleep_time:.1f}s"
                    )
                    time.sleep(sleep_time)

    sentiment_groups = group_by_sentiment(feed_data, combined_results, bot_handle)
    pretty_print_results(bot_handle, combined_results)

    candidate_posts, desired_likes = decide_likes(sentiment_groups, bot_handle)

    liked_indices = perform_likes(client, bot_handle, candidate_posts, desired_likes)

    append_results_to_csv(
        feed_data,
        combined_results,
        bot_handle,
        sample_number,
        liked_indices,
    )


def main():
    logins = load_logins(LOGINS_CSV)
    if not logins:
        print("No logins found in Logins_Test.csv")
        return

    print(f"Loaded {len(logins)} login(s) from {LOGINS_CSV}")

    sample_number = get_next_sample_number(OUTPUT_CSV)
    print(f"\nThis run will use sample_number={sample_number} for ALL bots.\n")

    for handle, password in logins:
        run_for_bot(handle, password, sample_number)


if __name__ == "__main__":
    main()
