import app._
import models.database.Database
import org.scalatra._
import javax.servlet.ServletContext

class ScalatraBootstrap extends LifeCycle {
  override def init(context: ServletContext) {
    println("\tInitiating scalatra server")
    val databaseDirname = try {
      sys.env("FL_DATABASE")
    } catch {
      case e: java.util.NoSuchElementException => "database"
    }
    println(s"\t Database directory: ${databaseDirname}")
    val db = new Database(databaseDirname)
    db.load  // Consider adding this into the initialization
    context.mount(new FreelanceandoServlet(db), "/*")
  }
}
