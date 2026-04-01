from typing import List, Dict, Any
from transformers import pipeline

class SentimentInitError(Exception):
    """Raised when the sentiment pipeline fails to initialize."""
    pass


class SentimentInferenceError(Exception):
    """Raised when sentiment inference fails on given texts."""
    pass


_sentiment_pipeline = None


def get_sentiment_pipeline():
    """
    Lazily initialize and return the sentiment analysis pipeline.
    Raises SentimentInitError if the pipeline cannot be created.
    """
    global _sentiment_pipeline
    if _sentiment_pipeline is None:
        try:
            _sentiment_pipeline = pipeline(
                "text-classification",
                model="cardiffnlp/twitter-roberta-base-sentiment-latest",
                truncation=True,
            )
        except (OSError, EnvironmentError, RuntimeError, ValueError) as e:
            raise SentimentInitError(
                f"Failed to initialize sentiment pipeline: {e}"
            ) from e
    return _sentiment_pipeline


def analyze_texts(texts: List[str]) -> List[Dict[str, Any]]:
    """
    Run sentiment analysis on a list of strings.

    Returns a list of dicts like:
    [
      {"label": "positive", "score": 0.98, ...},
      ...
    ]

    Raises SentimentInitError if the pipeline can't be created.
    Raises SentimentInferenceError if inference fails.
    """
    if not texts:
        return []

    cleaned = [str(t) for t in texts if str(t).strip()]
    if not cleaned:
        return []

    analyzer = get_sentiment_pipeline()

    try:
        results = analyzer(cleaned)
    except (RuntimeError, ValueError) as e:
        raise SentimentInferenceError(
            f"Sentiment inference failed: {e}"
        ) from e

    return results


def analyze_and_attach(texts: List[str]) -> List[Dict[str, Any]]:
    """
    Convenience helper: return a list of dicts with both the text and its sentiment.

    [
      {
        "text": "...",
        "label": "positive",
        "score": 0.98
      },
      ...
    ]

    If SentimentInitError or SentimentInferenceError bubble up, callers can
    catch them and decide how to handle (e.g., skip likes for this bot).
    """
    results = analyze_texts(texts)
    combined: List[Dict[str, Any]] = []

    for text, res in zip(texts, results):
        combined.append(
            {
                "text": text,
                "label": res["label"],
                "score": float(res["score"]),
            }
        )
    return combined
