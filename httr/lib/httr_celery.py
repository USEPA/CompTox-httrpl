
"""
defines the celery app to enable multi-processing

"""

import logging
import os

from celery import Celery

print(f"Using Celery broker: redis")

app = Celery(
    "httr",
    broker="redis://default:aoioieoue!@127.0.0.1:6379",
    backend="rpc://",
    include=["tasks"],
)


# Optional configuration, see the application user guide.
app.conf.update(
    result_expires=3600,
)

if __name__ == "__main__":
    print(f"celery app started")
    """
    starts the celery app to enable multi-processing
    """
    app.start()
