package com.evolutiongaming.bootcamp.basics

object ControlStructuresHomework1 {
  type Error = String

  // consider person an adult if their age is higher or equal to 18
  // return error message "$age is too high, are you human?" if age is higher than 150
  // return error message "$age is negative, we do not serve unborn people" if age is lower than 0
  // use if-else
  def isAdultIf(age: Int): Either[Error, Boolean] =
    if (age < 0) Left("%d is negative, we do not serve unborn people".format(age))
    else if (age > 150) Left("%d is too high, are you human?".format(age))
    else if (age > 17) Right(true)
    else Right(false)

  // same as isAdultIf, but use match statement instead
  def isAdultMatch(age: Int): Either[Error, Boolean] =
    age match {
      case age if age < 0 => Left("%d is negative, we do not serve unborn people".format(age))
      case age if age < 18 => Right(false)
      case age if age > 150 => Left("%d is too high, are you human?".format(age))
      case _ => Right(true)
    }

  // https://en.wikipedia.org/wiki/Triangle_inequality, consider degenerate triangles invalid
  // can you do it without using any control structures?
  def isValidTriangle(a: Double, b: Double, c: Double): Boolean = {
    val sides = List(a, b, c).sorted
    sides.head + sides(1) > sides(2)
  }

  // IT company located in Wakanda is searching for a new programmer. Due to high interest it needs
  // a way to filter out candidates that are not suitable for this job.
  // They have a number of characteristics that give candidates points, and valid candidate
  // should earn at least 10 points.
  //
  // * Each year of experience gives candidate 1 point, but not more than 5
  // * If candidate has education, it gives him 3 points
  // * Each passed test gives candidate 1 point starting from 6th passed test.
  //   If candidate has passed less than 5 tests, they don't want to hire him in any case.
  // * They prefer candidates from their country, so being from Wakanda gives candidate 3 points.
  //   If the candidate is from any of the neighboring countries - Narnia, Skyrim or Amestris -
  //   candidate should get 1 point, otherwise - 0
  // * Summed stars on github also give points.
  //   1 point if candidate has 10 stars, 2 points for 100, 3 points for 1000 and so on,
  //   giving 1 point for each WHOLE new power of ten, e.g. 9 is still 0 points, 99 is still 1 point.
  //
  // All input is valid, e.g. candidate can't have negative years of experience
  def isValidCandidate(
                        country: String,
                        passedTests: Int,
                        yearsOfExperience: Int,
                        hasEducation: Boolean,
                        starsOnGithub: Int
                      ): Boolean = {
    var points = 0
    var isValid = false
    val neighboringCountries = List("Narnia", "Skyrim", "Amestris")
    if (passedTests < 5) return isValid

    if (country == "Wakanda") points += 3
    if (neighboringCountries.contains(country)) points += 1
    if (passedTests > 5) points += passedTests - 5
    if (yearsOfExperience < 5) points += yearsOfExperience
    if (yearsOfExperience >= 5) points += 5
    if (hasEducation) points += 3
    if (Math.floor(Math.log10(starsOnGithub)).toInt >= 1) points += Math.floor(Math.log10(starsOnGithub)).toInt

    if (points > 9) isValid = true

    isValid
  }
}
