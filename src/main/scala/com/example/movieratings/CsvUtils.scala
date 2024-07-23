package com.example.movieratings

import org.apache.commons.csv.{CSVFormat, CSVRecord}

import java.io.{File, FileReader, FileWriter}
import scala.collection.JavaConverters._

/** Helper methods to work with CSV files.
 *
 * more info: https://commons.apache.org/proper/commons-csv
 */
object CsvUtils {

  /** Reads records from a CSV file.
   *
   * Examples:
   * val record: CSVRecord = ...
   * record.size() // the number of columns in this record
   * record.get(2) // returns the third column as a String
   */
  def readFromFileAsList(file: File): List[CSVRecord] = {
    val reader = new FileReader(file)
    try {
      CSVFormat.DEFAULT.parse(reader).asScala.toList
    } finally {
      reader.close()
    }
  }

  /** Writes records to a file in a CSV format.
   *
   * Each record is a List[Any], e.g. this snippet writes two records to a file:
   * val records = List(
   *   List("John", 41, "Plumber"),
   *   List("Misato-san", 29, "Operations Director")
   * )
   * CsvUtils.writeToFile(records, new File("employees.csv"))
   */
  def writeToFile(records: Iterable[List[Any]], file: File): Unit = {
    val writer = new FileWriter(file)
    try {
      val printer = CSVFormat.DEFAULT.print(writer)
      records.foreach(r => printer.printRecord(r.asJava))
      printer.close(true)
    } finally {
      writer.close()
    }
  }

}
