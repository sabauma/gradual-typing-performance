"""
Project-wide constants and settings
"""

# If <=0, print no debugging information.
# Otherwise, print more.
# A higher number means more printouts, up to an un-documented max.
DEBUG = 0

# Separator for .tag and .graph files
SEP = "\t"

# Place to store intermediate results, when sampling.
TMP_RESULTS = "summarize-tmp-results.txt"

# Directory to save outputs
OUTPUT_DIR = "output-summary"

CACHE = 0
RECOMPUTE = 1
APPEND = 2

# Rough constants, to help orient the graphs in a larger picture
DELIVERABLE = 2
ACCEPTABLE = 10
