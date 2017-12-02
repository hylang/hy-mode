import hy  # This import is mandatory for pytest to load hy modules.
from _pytest.python import Module


def pytest_collect_file(path, parent):
    if path.ext == ".hy" and "test_" in path.basename:
        return Module(path, parent)
