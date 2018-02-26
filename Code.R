
# # Heading 1

# ## Heading 2

# ### Heading 3

# Plain text

# *Emphasized text*

# **Bold text**

# `Monospaced text`

# Bulleted list
# * Item
#   * Subitem
# * Item

# Numbered list
# 1. First Item
# 2. Second Item
# 3. Third Item

# [link](https://www.cloudera.com)


# ## Copying Files to HDFS

# This project includes a dataset describing on-time
# performance for flights departing New York City airports
# (EWR, JFK, and LGA) in the year 2013. This data was
# collected by the U.S. Department of Transportation. It
# is stored here in a comma-separated values (CSV) file
# named `flights.csv`.

# Copy this file to HDFS by running `hdfs dfs` commands in
# CDSW using the R function `system()`:

# Delete the `flights` subdirectory and its contents in
# your home directory, in case it already exists:

system("hdfs dfs -rm -r flights")

# Create the `flights` subdirectory:
