from setuptools import find_packages, setup

setup(
    name="resonance-log-viewer",
    version="0.1.0",
    description="Organic AI monitoring dashboard with cosmic laws and endocrine system",
    packages=find_packages(),
    python_requires=">=3.8",
    install_requires=[
        "Flask",
        "PyYAML",
        "requests",
        "chartjs",  # if needed; else remove
    ],
)
