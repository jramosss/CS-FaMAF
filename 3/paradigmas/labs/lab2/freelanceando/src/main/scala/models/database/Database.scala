package models.database

import models._
import java.sql.ClientInfoStatus

/* This is a Database abstraction.
 * A Database is a collection of different tables that are loaded into memory.
 */
class Database (val databaseDirname: String) {

  /* Add your own tables here
   * This attribute of the Database keeps track of the existing tables and
   * the constructor (companion object) used to create objects from the
   * information saved in that table.
   */

  // We create a single instance of each table, and then use it when loading
  // and querying instances.
  val categories = new DatabaseTable[Category](
    s"${databaseDirname}/categories.json")
  
  val freelancers = new DatabaseTable[Freelancer](
    s"${databaseDirname}/freelancers.json")

  val clients = new DatabaseTable[Client](
    s"${databaseDirname}/clients.json")
  
  val jobs = new DatabaseTable[Job](
    s"${databaseDirname}/jobs.json")

  def load: Unit = {
    categories.load(Category)
    freelancers.load(Freelancer)
    clients.load(Client)
    jobs.load(Job)
  }
}
