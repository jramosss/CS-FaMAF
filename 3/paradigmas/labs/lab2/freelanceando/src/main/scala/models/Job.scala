package models

import org.json4s._
import org.json4s.DefaultFormats
import org.json4s.JsonAST.JArray


/* Object companion of class Category */
object Job extends ModelCompanion[Job] {

  implicit lazy val formats = DefaultFormats

  /* This class constructor needs to be overwritten in every subclass of
   * ModelCompanion, because it needs the direct reference to each subclass,
   * in this case, Category.
   */
  def apply: Job = new Job
}

class Job extends Model[Job] {

    protected [models] var title: String = ""
    
    protected [models] var categoryId: Int = 0
    
    protected [models] var clientId: Int = 0

    protected [models] var preferredExpertise: String = ""

    protected [models] var preferredCountry: String = ""

    protected [models] var hourlyPrice: Float = 0

    def getTitle: String = this.title

    def getCategoryId: Int = this.categoryId
    
    def getClientId: Int = this.clientId

    def getPreferredExpertise: String = this.preferredExpertise

    def getPreferredCountry: String = this.preferredCountry

    def getHourlyPrice: Float = this.hourlyPrice

    override def toMap: Map[String,Any] = super.toMap.+(("title",this.title),
      ("category_id",this.categoryId), 
      ("client_id",this.clientId), 
      ("preferred_expertise",this.preferredExpertise),
      ("preferred_country",this.preferredCountry), 
      ("hourly_price",this.hourlyPrice)
      )

    override def fromJson(jsonValue: JValue): Job = {
        
      super.fromJson(jsonValue)

      (jsonValue \ "title") match {
        case JString(value) => this.title = value.toString
        case _ =>
      }

      (jsonValue \ "category_id") match {
        case JInt(value) => this.categoryId = value.toInt
        case _ =>
      }

      (jsonValue \ "client_id") match {
        case JInt(value) => this.clientId = value.toInt
        case _ =>
      }

      (jsonValue \ "preferred_expertise") match {
        case JString(value) => this.preferredExpertise = value.toString
        case _ =>
      }

      (jsonValue \ "preferred_country") match {
        case JString(value) => this.preferredCountry = value.toString
        case _ =>
      }

      (jsonValue \ "hourly_price") match {
        case JDouble(value) => this.hourlyPrice = value.toFloat
        case _ =>
      }

      this
    }
}