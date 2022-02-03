package models

import org.json4s.{DefaultFormats, JValue, JInt, JString}
import org.json4s.DefaultFormats
import org.json4s.JsonAST.JArray


/* Object companion of class Category */
object Category extends ModelCompanion[Category] {

  implicit lazy val formats = DefaultFormats

  /* This class constructor needs to be overwritten in every subclass of
   * ModelCompanion, because it needs the direct reference to each subclass,
   * in this case, Category.
   */
  def apply: Category = new Category
}

class Category extends Model[Category] {

  protected[models] var name: String = ""
  //override protected[models] var id: Int = 0

  def getValue = name
  //override def getId: Int = id

  override def toMap: Map[String, Any] = super.toMap.+(("name", name))

  override def fromJson(jsonValue: JValue): Category = {

    (jsonValue \ "name") match {
      case JString(value) => name = value.toString
      case _ =>
    }
    
    super.fromJson(jsonValue)
  }

}
