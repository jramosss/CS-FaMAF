package models

import org.json4s._
import org.json4s.DefaultFormats
import org.json4s.JsonAST.JArray


/* Object companion of class Category */
object Client extends ModelCompanion[Client] {

  implicit lazy val formats = DefaultFormats

  /* This class constructor needs to be overwritten in every subclass of
   * ModelCompanion, because it needs the direct reference to each subclass,
   * in this case, Category.
   */
  def apply: Client = new Client
}

class Client extends Model[Client] with Person {
//class Client extends Person[Client] {
  private var totalSpend: Double = 0

  protected[models] var jobsIds: List[Int] = List()

  def getTotalSpend: Double = this.totalSpend

  def getJobsIds: String = {
    var str: String = "["
    jobsIds.foreach(y => str = str.concat( y + ","))
   
    str = str.slice(0,(str.length()-1))
    str = str.concat("]")
    
    str
  }


  def setNewJob(id:Int) = {
    this.jobsIds:::= List(id)
  }

  def pay(amount:Double, fl: models.Freelancer) = {
    this.totalSpend += amount
    fl.totalEarnings += amount
  }

  override def toMap: Map[String,Any] = super.toMap.+(
    ("username",this.username), 
    ("country_code",this.countryCode), ("total_spend",this.totalSpend), 
    ("jobs_ids",this.jobsIds)
    )

  override def fromJson(jsonValue: JValue): Client = {

    (jsonValue \ "username") match {
      case JString(value) => this.username = value.toString
      case _ => 
    }
    
    (jsonValue \ "country_code") match {
      case JString(value) => this.countryCode = value.toString
      case _ =>
    }

    (jsonValue \ "total_spend") match {
      case JDouble(value) => this.totalSpend = value.toDouble
      case _ =>
    }

    (jsonValue \ "jobs_ids") match {
      case JArray(value) => value.foreach(x => 
        x match {
          case JInt(value) => this.jobsIds = this.jobsIds.::(value.toInt)
          case _ =>  
        })
      case _ =>
    }
    
    super.fromJson(jsonValue)
  }
} 