from contextlib import contextmanager
from time import time


@contextmanager
def report_time():
    start = time()
    yield
    end = time()
    duration = end - start
    print(f'Took {duration:.3f}s')
