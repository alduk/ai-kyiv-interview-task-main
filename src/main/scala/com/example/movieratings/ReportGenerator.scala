package com.example.movieratings

import java.io.File
import scala.util.{Failure, Success, Try}

case class Movie(id: Int, title: String, year: Int)

case class MovieStats(movie: Movie, totalRating: Int, numReviews: Int) {
  def averageRating: Double = totalRating.toDouble / numReviews
}

object ReportGenerator {

  def main(args: Array[String]): Unit = {
    // check out CsvUtils for methods to read and write CSV files
    if (args.length != 3) {
      println("Usage: ReportGenerator <movie-titles-file> <training-set-dir> <output-report-file>")
      sys.exit(1)
    }
    val movieTitlesFile = args(0)
    val trainingSetDir = new File(args(1))
    if (!trainingSetDir.exists() && !trainingSetDir.isDirectory) {
      println(s"Training set directory $trainingSetDir ot exist or is not a directory")
      sys.exit(1)
    }
    val outputReportFile = args(2)
    val statsFilter = (stat: MovieStats) => stat.movie.year >= 1970 && stat.movie.year <= 1990 && stat.numReviews > 1000
    Try {
      val movies = readMovies(new File(movieTitlesFile))
      val movieStats = processRatings(trainingSetDir, movies, statsFilter)
      val sortedStats = sortMovieStats(movieStats)
      writeReport(sortedStats, new File(outputReportFile))
    } match {
      case Failure(exception) =>
        println(s"An error occurred: ${exception.getMessage}")
        sys.exit(1)
      case Success(value) => sys.exit(0)
    }
  }

  def readMovies(file: File): Map[Int, Movie] = {
    CsvUtils.readFromFileAsList(file).flatMap { record =>
      Try {
        val id = record.get(0).toInt
        val year = record.get(1).toInt
        val title = record.get(2)
        id -> Movie(id, title, year)
      }.toOption
    }.toMap
  }

  def processRatings(dir: File, movies: Map[Int, Movie], filter: MovieStats => Boolean): Map[Int, MovieStats] = {
    dir.listFiles().map { file =>
      val movieId = file.getName.drop(3).dropRight(4).toInt
      val stats = movies.get(movieId).map { movie =>
        val fileSource = scala.io.Source.fromFile(file)
        val lines = fileSource.getLines().drop(1)
        val (totalRating, numReviews) = lines.foldLeft((0, 0)) { (acc, line) =>
          val rating = line.split(",")(1).toInt
          (acc._1 + rating, acc._2 + 1)
        }
        MovieStats(movie, totalRating, numReviews)
      }
      println(s"Processed ${file.getName} with $stats")
      (movieId, stats)
    }.toMap.collect {
      case (key, Some(stats)) if filter(stats) => key -> stats
    }

  }

  def sortMovieStats(stats: Map[Int, MovieStats]): List[MovieStats] = {
    stats.values.toList.sortBy(stat => (-stat.averageRating, stat.movie.title))
  }

  def writeReport(stats: List[MovieStats], outputFile: File): Unit = {
    val records = stats.map { stat =>
      List(
        stat.movie.title,
        stat.movie.year.toString,
        f"${stat.averageRating}%.2f",
        stat.numReviews.toString
      )
    }
    CsvUtils.writeToFile(records, outputFile)
  }
}