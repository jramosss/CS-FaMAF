package models.database

import java.io._

import org.json4s.{JInt,JNothing,JString}
import org.json4s.JsonAST.{JArray,JValue}
import org.json4s.DefaultFormats
import org.json4s.jackson.JsonMethods.parse
import org.json4s.jackson.Serialization

import scala.collection.mutable.{Map => Dict}
import scala.io.Source
import scala.util.{Success, Try, Failure}

import models._

case class ModelWithoutId(message: String) extends Exception(message)

case class IntList(value: List[Int])

/* DatabaseTable is a polimorphic class that receives a subclass of Model as
 * type parameter.
 */
class DatabaseTable[M <: Model[M]](val filename: String) {

  implicit lazy val formats = DefaultFormats  // Ignore this for now

  /* This is the map from id to instances that we maintain in memory.
   * It's a diccionary so we can search for instances by their id in constant
   * time, instead of linear.
   */
  private val _instances: Dict[Int, M] = Dict()

  /* Return the next free id in the table. */
  private def getNextId: Int =
    if (_instances.isEmpty) 1 else _instances.keys.max + 1

  /* Return an immutable copy of the list of instances. */
  def all: List[M] = {
    var res : List[M] = List()
    _instances.map(y => res :::= List(y._2))
    
    res
  }

  /* Return Some(instance) if there is an instance in _instances with @id.
   * Return None otherwise.
   */
  def get(id: Int): Option[M] = {
    val bound = _instances.keys.max
    id match {
      case x if x > bound =>  None
      case x if x <= bound => _instances.get(id)
    }
  }

  /* Return a list of instances that matches the pairs
   * (attributeName, attributeValue) in @attributes map. Instances must match
   * ALL attributes exactly. Value types may differ. For example, an instance
   * with attribute `earnings = 10` would match the @attributes
   * `Map("earnings" -> "10")`
   */
  def filter(attributes: Map[String, Any]): List[M] = {
    var lres:List[M] = List()
    var flag = 1
    var cant_atrib = 0
    var fns:List[M] =List()
    _instances.foreach(p => { 
      cant_atrib = 0
      attributes.foreach(x => {
        x._2 match {
          /*
          Si el valor del atributo es una lista(category_ids), 
          entonces tenemos que checkear si alguna de los elementos
          de alguna lista de alguna instancia es igual
          */
          case IntList(a) => p._2.toMap.get(x._1).get match {
            case IntList(g) => {
              a.foreach(f => {
                if(!g.contains(f)){
                  flag = 0
                }
              })
              if (flag == 1){
                cant_atrib += 1
              }
            }
          }
          case _ => { //Si no es una lista, simplemente checkeamos
                      //si alguna instancia cumple el valor en ese atributo
            if((p._2.toMap.get(x._1)) == Some(x._2)) {
              cant_atrib += 1
            }
          }
        }
      })
      if (cant_atrib == attributes.size) {
        lres :::= List(p._2)
      }
    })
    lres
  }

  def valid_client(cli:JValue) = {
    var res = 0
    cli match {
      case JInt(value) => {
        if (get(value.toInt) != None) {
          res = 1
        } 
      }
      case _ =>
    }
    res
  }

  def valid_categories(cat:List[JValue]):Int = {
    var res = 1
    cat.foreach(x => {
      x match {
        case JInt(value) => {
          if ((get(value.toInt) != None) && (res == 1)) {
          res = 1
          } else {
            res = 0
          }
        }
        case _ => 
    }
    })
    res
  }

  /* ** YOU DON'T NEED TO TOUCH ANYTHING BELOW THIS LINE **
   * (unless you are doing extra exercises)
   */

  def loadJsonFile: Try[List[JValue]] = Try {
    println(s"\t Loading table ${filename}")
    val source = Source.fromFile(filename)
    val jsonData = source.getLines.mkString
    source.close()
    val parsedData = parse(jsonData)
    parsedData.extract[List[JValue]]
  }

  /* Loads the instances saved in Json format into the instances list.
   * We need to explicitely pass the className (which is the same as M), to
   * have a constructor to build the objects. This happens because M, as a type
   * parameter, can't be accesed at compile time.
   */
  def load[MC <: ModelCompanion[M]](className: MC): Unit = {
    loadJsonFile match {
      case Success(jsonList) =>
        jsonList.foreach { jv => _add(className.apply.fromJson(jv)) }
      case Failure(exception) =>
        println(s"\tERROR loading table ${filename}\n\t${exception}")
    }
  }

  /* Adds an instance without saving to the database.
   */
  def _add(instance: M): Try[Unit] = {
    instance.getId match {
      case 0 => {
        val errorMsg = s"\t\t! Attempting to save ${instance.toMap} with id 0."
        println(errorMsg)
        Failure(ModelWithoutId(errorMsg))
      }
      case _ => {
        println(s"\t\tAdding instance ${instance.toMap}")
        _instances(instance.getId) = instance
        Success()
      }
    }
  }

  def delete(id: Int): Unit = {
    _instances.remove(id)
    write match {
      case Failure(_) => println(s"Unable to save deleted instance ${id}")
      case Success(_) =>  // just keep doing stuff
    }
  }

  def save(instance: M): Unit = {
    instance.getId match {
      case 0 => { instance.id = this.getNextId }
      case _ =>
    }
    _add(instance)
    // Here we could add consistency checks before writing.
    write match {
      case Failure(_) => println(s"Unable to save instance ${instance.getId}")
      case Success(_) =>  // just keep doing stuff
    }
  }

  def write: Try[Unit] = Try {
    val pw = new PrintWriter(new File(filename))
    pw.write(Serialization.write(_instances.values.map(model => model.toMap)))
    pw.close()
  }

}
