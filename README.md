# ai-kyiv-interview-task

## Buiding and running

To compile and build this app, run the following command:
```shell
./sbt compile test package
```

A command to run this app:
```shell
./sbt "runMain com.example.movieratings.ReportGenerator /path/to/movie_titles.txt /path/to/training_set /path/to/output-report.csv"
```

## Requirements

Implement a Scala application that generates a CSV report with movie ratings.

### Input data

Download the [Netflix Prize Datset](https://academictorrents.com/details/9b13183dc4d60676b773c9e2cd6de5e5542cee9a)

The Netflix Prize dataset contains the following files that are needed for this task:

* Training Dataset Files
  - The file "training_set.tar" is a tar of a directory containing 17770 files, one per movie (NOTE: unpack this tar file).
  - Each file name has a format like `mv_<MOVIE_ID>.txt`, e.g. `mv_0014067.txt` (NOTE: movie IDs in file names are zero-padded to 7 digits)
  - The first line of each file contains the movie id followed by a colon.  Each subsequent line in the file corresponds
    to a rating from a customer (review) and its date in the following format:
    ```CustomerID,Rating,Date```
  - `MovieID`s range from 1 to 17770 sequentially.
  - `CustomerID`s range from 1 to 2649429, with gaps. There are 480189 users.
  - Ratings are on a five star (integral) scale from 1 to 5.
  - Dates have the format `YYYY-MM-DD`
* Movies Description File
  - Movie information in "movie_titles.txt" is in the following format:
    ```MovieID,YearOfRelease,Title```
  - `YearOfRelease` can range from 1890 to 2005.

### Task requirements

Prepare a CSV report with the following columns:
* movie title
* year of release of this movie
* average rating (NOTE: real number, not integer)
* number of reviews

Report entries should be sorted by the average rating in descending order (from the highest rating to the lowest rating).
If two movies have the same rating, break ties by sorting on the movie title.

Add a movie to this report only if it satisfies the following conditions:
* the movie year of release is between 1970 and 1990
* the number of reviews is greater than 1000

Your application should accept the following command-line arguments:
* a path to the movies description file
* a path to the training dataset directory
* a report output path

Technical requirements:
* use Scala
* use sbt to build your project (provided in a skeleton; see build and run instructions above)
* do not use any data processing frameworks like Spark; it should be a simple Scala app
