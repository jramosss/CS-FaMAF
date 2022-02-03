package models

import org.json4s._
import org.json4s.DefaultFormats
import org.json4s.JsonAST.JArray


/* Object companion of class Category */
object Freelancer extends ModelCompanion[Freelancer] {

  implicit lazy val formats = DefaultFormats

  /* This class constructor needs to be overwritten in every subclass of
   * ModelCompanion, because it needs the direct reference to each subclass,
   * in this case, Category.
   */
  def apply: Freelancer = new Freelancer
}

class Freelancer extends Model[Freelancer] with Person {
//class Freelancer extends Person[Freelancer] {
  
  protected[models] var reputation: String = ""
  
  protected[models] var categoryIds: List[Int] = List()

  protected[models] var hourlyPrice: Double = 0.0

  protected[models] var totalEarnings: Double = 0

  def getReputation: String = reputation
  
  def getHourlyprice: Double = hourlyPrice
  
  def getCategoryids: String = {
    var str: String = "["
    categoryIds.foreach(y => str+=(y+","))
    str = str.slice(0,(str.length()-1))
    str += "]"
    str
  }
  
  def getTotalEarnings: Double = totalEarnings
  
  def setReputation(rep:String) = {
    this.reputation = rep
  }

  override def toMap: Map[String, Any] = super.toMap.+(
    ("username", this.username),
    ("country_code", this.countryCode),
    ("category_ids", this.categoryIds),
    ("reputation", this.reputation),
    ("hourly_price", this.hourlyPrice),
    ("total_earnings", this.totalEarnings)
    )

  override def fromJson(jsonValue: JValue): Freelancer = {

    (jsonValue \ "username") match {
      case JString(value) => this.username = value.toString
      case _ =>
    }
    (jsonValue \ "country_code") match {
      case JString(value) => this.countryCode = value.toString
      case _ =>
    }
    (jsonValue \ "category_ids") match {
      case JArray(value) => value.foreach(_ match {
        case JInt(value) => this.categoryIds = this.categoryIds.:: (value.toInt)
        })
      case _ =>
    }
    (jsonValue \ "reputation") match {
      case JString(value) => this.reputation = value.toString
      case _ =>
    }
    (jsonValue \ "hourly_price") match {
      case JDouble(value) => this.hourlyPrice = value.toDouble
      case _ =>
    }

    (jsonValue \ "total_earnings") match{
      case JDouble(value) => this.totalEarnings = value.toDouble
      case _ =>
    }

    super.fromJson(jsonValue)
  }
}
