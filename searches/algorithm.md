## Algorithm of Searching
* Initialize an index variable to track the position where the value to search is found (set to an initial value, typically -1 to indicate not found).

* Start looking through the data, comparing each element with the value to search.

* If a match is found, update the index variable to the current position and stop searching.

* Continue searching until the end of the data or until a match is found.

* After the search, if the index remains at its initial value, it means the value to search was not found, so return a special value (e.g., -1) and taking this into consideration output an appropriate line for value not found. Otherwise, return the index where the search key was found.