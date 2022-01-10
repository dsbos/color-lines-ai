package com.us.dsb.explore.enums.enumeratum

import enumeratum.{Enum, EnumEntry}

import scala.util.chaining.scalaUtilChainingOps


/**
 * Exploration of common things on enumerations of table columns.
 *
 * NOTE:  See end of file for (current) main point of doing all this.
 */

object MultiTableColumnsEnumExpl extends App {

  // Note: Trying semantically accurate different "column" vs. "columns" on
  //   things that may typically be named the same (instances of "column" vs. lists
  //   of columns.

  object ImplementationIndependent {  // (Some of these would be packages.)

    // (Note:  Was trait before addition of aux and passing aux through chain of
    // constructors).
    /** Table column.  (Not necessarily implemented via `Enum`/`EnumEntry`.) */
    abstract class BaseTableColumn(aux: String) {

      /**
       * Gets simple (unqualified) SQL name of this column.
       */
      def toSqlSimpleName: String

      // Maybe other column characteristics, perhaps usual corresponding exposed
      //  column name?
      def someAuxThing: String = aux
    }

    object BaseTableColumn {

      // NOTE: The following implicit conversion could be moved from
      // BaseTableColumn's companion object to other object from where it could
      // be imported only explicitly.

      import scala.language.implicitConversions
      import doobie.Fragment
      import doobie.syntax.SqlInterpolator.SingleFragment
      /**
       * Implicit conversion to a [[Fragment]] for simple (unqualified) column
       * name reference, for succinct use of [[BaseTableColumn]] in Doobie
       * `fr"..."` and `sql"..."` literals.
       *
       * Automatically converts to make it easy to use the name in Doobie `fr`
       * and `sql` literals as SQL syntax (i.e., column name) rather than as a
       * data (string) value, specifically, so that:
       *   1. use can be simply `\$some_column` rather than
       *      something like harder-to-read `\${some_column.asFrag}`, while
       *   1. column declarations can be enumeration values (etc)., with other
       *      properties too rather then each being like
       *      'val some_column = Fragment.const("some-column")' and only being
       *      useful as a Fragment).
       *
       * (An implicit conversion is used because Doobie doesn't seem to have
       * any implicit-parameter/typeclass way of doing this for SQL (vs. data
       * values).)
       */
      implicit def toFragment(c: BaseTableColumn): SingleFragment[BaseTableColumn] =
        Fragment.const0(c.toSqlSimpleName)
    }


    // Some application-specific special characteristics of some table columns:

    /**
     * Table column to be included in current simple text search.
     * (Not necessarily implemented via `Enum`/`EnumEntry`.)
     */
    trait TextSearchableColumn extends BaseTableColumn

    /** Table column chosen as name of things in table.
     * (Not necessarily named `name`; not necessarily implemented via `Enum`/`EnumEntry`.)
     */
    trait NameColumn extends BaseTableColumn with TextSearchableColumn // always searchable

    //

    /** List of columns for a table.
     * (Not necessarily named `name`; not necessarily implemented via `Enum`/`EnumEntry`.)
     */
    trait BaseTableColumnsList {

      // Some application-specific special characteristics/properties of tables:

      /** Gets this column set's logical name column. */
      val getNameColumn: NameColumn

      // ?? TODO:  Resolve:  Only commit to BaseTableColumn, or narrow to TextSearchableColumn
      /** Gets this column set's columns to be text-searched. */
      val getSearchColumns: IndexedSeq[BaseTableColumn]
    }

    // ???? TODO: Move to generic part?
    // TODO:  Any need to be trait?  (Class allows easier subclassing.)
    abstract class BaseTable(val sqlSimpleName: String) {
      val getColumns: BaseTableColumnsList
      lazy val getNameColumn: NameColumn = getColumns.getNameColumn // ???? Check best "lazy" place
    }

  }

  object EnumImplementation {
    import ImplementationIndependent._

    // (Note:  Was trait before addition of aux and passing aux through chain of
     // constructors).
    /** Table column implemented with Enumeratum. */
    abstract class EnumTableColumn(aux: String) extends BaseTableColumn(aux) with EnumEntry {

      /**
       * @inheritdoc
       * This implementation delegates to `entryName`.  Note that that works
       * only if that value is a valid regular SQL identifer.  Otherwise, the
       * column needs to override this method. (Or this need to be re-written to
       * recognize and properly quote irregular identifiers.)
       */
      override def toSqlSimpleName: String = entryName
    }

    /** List of columns for a table, implemented with Enumeratum. */
    trait EnumTableColumnsList[TC <: EnumEntry] extends BaseTableColumnsList with Enum[TC] {

      /**
       * @inheritdoc
       * This implementation gets the one NameColumn column.
       */
      override lazy val getNameColumn: NameColumn = {
        values.flatMap { col =>
          col match {
            case nameColumn: NameColumn => Some(nameColumn)
            case _ => None
          }
        }
        .tap { found =>
          assert( found.size <= 1,
                  s"Multiple NameColumn columns found: ${found.mkString(", ")}.")
        }
        .headOption
        .getOrElse(throw new NoSuchElementException("No NameColumn column found."))

        // Note:  Can't check at initialization time for having multiple name
        // columns , because this getNameColumn has to be lazy to not execute
        // before "values" is initialized (later, since on subclass of this
        // trait?).  (Removing "lazy" above leads to NullPointerException.)
      }

      /**
       * @inheritdoc
       * This implementation lists all the TextSearchableColumn columns.
       */
      override lazy val getSearchColumns: IndexedSeq[TextSearchableColumn] = {
        values.flatMap { col =>
          col match {
            case searchColumn: TextSearchableColumn => Some(searchColumn)
            case _ => None
          }
        }
      }
    }

  }

  object Tables {
    import ImplementationIndependent.NameColumn
    import ImplementationIndependent.TextSearchableColumn
    import ImplementationIndependent.BaseTableColumnsList
    import ImplementationIndependent.BaseTable
    import EnumImplementation.EnumTableColumn
    import EnumImplementation.EnumTableColumnsList

    /** Users table metadata. */
    object UsersTable extends BaseTable("users") {
      // (Note:  These were traits before addition of aux and passing aux through
      // chain of constructors).
      /** A column in the users table. */
      sealed abstract class Column(aux: String) extends EnumTableColumn(aux) with EnumEntry

      /** Declares the columns in the users table. */
      object Columns extends EnumTableColumnsList[Column] {

        case object name  extends Column("Name") with NameColumn
        case object email extends Column("E-Mail Address") with TextSearchableColumn
        case object other extends Column("Something Else")
        // Big sequence of other columns goes here.

        override val values = findValues
      }
      override val getColumns = Columns
    }

    object GroupsTable extends BaseTable("groups") {
      sealed abstract class Column(aux: String) extends EnumTableColumn(aux) with EnumEntry

      object Columns extends EnumTableColumnsList[Column] {

        case object name      extends Column("Group Name") with NameColumn
        case object something extends Column("Something Name")
        //case object abnormal        extends GroupsColumn with NameColumn
        // Big sequence of other columns goes here.

        override val values = findValues
      }
      override val getColumns = Columns
    }

    object OtrosTabla extends BaseTable("otras_cosas") {
      sealed abstract class TablaColumna(aux: String) extends EnumTableColumn(aux) with EnumEntry

      object Columnas extends EnumTableColumnsList[TablaColumna] {

        case object nombre        extends TablaColumna("Name/Nombre") with NameColumn
        case object cosa_de_texto extends TablaColumna("Textual Thing") with TextSearchableColumn
        case object otra_cosa     extends TablaColumna("Something Else")
        // Big sequence of other columns goes here.

        override val values = findValues
      }
      override val getColumns = Columnas
    }
  }

  object ExpositoryClientCode {
    import ImplementationIndependent.BaseTableColumn
    import ImplementationIndependent.NameColumn
    import ImplementationIndependent.BaseTableColumnsList
    import ImplementationIndependent.BaseTable

    import Tables.UsersTable
    import Tables.UsersTable
    import Tables.GroupsTable
    import Tables.OtrosTabla

    val usersColumnsGenerically: BaseTableColumnsList = UsersTable.Columns
    val usersTableGenerically: BaseTable = UsersTable

    println("UsersTable.Columns.name     = " + UsersTable.Columns.name)
    println("GroupsTable.Columns.name    = " + GroupsTable.Columns.name)
    //println("OtrosTabla.Columnas.name    = " + OtrosTabla.Columnas.name)  // different
    println("OtrosTabla.Columnas.name    = " + OtrosTabla.Columnas.nombre)
    //println("usersTableGenerically.name = " + usersTableGenerically.name)  // no specific cols.

    println("UsersTable.Columns.getNameColumn      = " + UsersTable.Columns.getNameColumn)
    println("GroupsTable.Columns.getNameColumn     = " + GroupsTable.Columns.getNameColumn)
    println("OtrosTabla.Columnas.getNameColumn     = " + OtrosTabla.Columnas.getNameColumn)
    println("usersColumnsGenerically.getNameColumn = " + usersColumnsGenerically.getNameColumn)
    println("usersTableGenerically.getNameColumn   = " + usersTableGenerically.getNameColumn)
    assert(OtrosTabla.Columnas.getNameColumn.toSqlSimpleName == "nombre")

    println()

    println("UsersTable.Columns.values            = " + UsersTable.Columns.values)
    assert(UsersTable.Columns.values ==
               Vector(
                 UsersTable.Columns.name,
                 UsersTable.Columns.email,
                 UsersTable.Columns.other
                 ))

    println("UsersTable.Columns.getSearchColumns  = " + UsersTable.Columns.getSearchColumns)
    assert(UsersTable.Columns.getSearchColumns ==
               Vector(
                 UsersTable.Columns.name,
                 UsersTable.Columns.email
                 ))
    println("OtrosTabla.Columnas.getSearchColumns = " + OtrosTabla.Columnas.getSearchColumns)
    assert(OtrosTabla.Columnas.getSearchColumns ==
               Vector(
                 OtrosTabla.Columnas.nombre,
                 OtrosTabla.Columnas.cosa_de_texto
                 ))

    // Doobie string interpolation:

    import doobie.Fragment
    import doobie.Fragments
    import doobie.implicits.toSqlInterpolator

    import UsersTable.Columns.name
    val value = "whatever"

    // Note non-quoting of column name and ~quoting/special handling of string
    // value (see implicit conversion (currengly) in object BaseTableColumn):

    println()
    println("""fr"\$name ILIKE '$value1'" = """ + fr"$name ILIKE '$value'" )
    assert(fr"$name ILIKE '$value'".toString == "Fragment(\"name ILIKE '?' \")")

    // Single method replacing the multiple andSearchSatisfies methods in admin-import-service:

    def makeSearchSql(rawSearchTerm: String, cols: BaseTableColumnsList): Fragment = {
      val searchTerm = s"%$rawSearchTerm%"
      val ilikeFrags = cols.getSearchColumns.map(c => fr"$c ILIKE '$searchTerm'")
      val orFrag = Fragments.or(ilikeFrags: _*)
      orFrag
      // ?? TODO:  Prototype how we use "AND"--or a fixed (more logical) way of
      //  ANDing subexpressions.
    }

    println(s"makeSearchSql(\"findme\", usersTableGenerically.getColumns) = " +
                makeSearchSql("findme", usersTableGenerically.getColumns))
    assert(makeSearchSql("findme",
                         usersTableGenerically.getColumns).toString ==
      """Fragment("(name ILIKE '?' ) OR (email ILIKE '?' ) ")""")


    // Auxiliary per-column data, perhaps default exposed column name:

    println()
    println("UsersTable.Columns.name.someAuxThing = " + UsersTable.Columns.name.someAuxThing)
    assert(UsersTable.Columns.name.someAuxThing == "Name")

    println("UsersTable.Columns.values = " +
                UsersTable.Columns.values)
    println("UsersTable.Columns.values.map(_.someAuxThing) = " +
                UsersTable.Columns.values.map(_.someAuxThing))
    assert(UsersTable.Columns.values.map(_.someAuxThing) ==
               Vector(UsersTable.Columns.name.someAuxThing,
                      "E-Mail Address",
                      UsersTable.Columns.other.someAuxThing))

  }
  ExpositoryClientCode


  ////////////////////////////////////////////////////////////////////////////////
  ////////////////////////////////////////////////////////////////////////////////
  ////////////////////////////////////////////////////////////////////////////////
  // AND NOW, the main point of all the above!:

  object TableSpecificClientCode {
    import doobie.implicits.toSqlInterpolator

    // Imagine the following multiple SQL commands or fragments referring to the
    // same columns are scattered around different classes (e.g., admin-ingest-service's
    // multiple xxxUserRepository classes providing different views on the users
    // table.
    //
    // Now, in a typical IDE, focus on a use of a column declaration
    // in a SQL fragment below, jump to the declaration (command-B in Mac
    // IntelliJ IDEA), and then jump to one of, or list all, the references in
    // (similarly coded) SQL fragments (command-B or option-F7, respectively,
    // in Mac IDEA). You can't usually do those traversals with text search!

    object SomeUsersThing {
      import Tables.UsersTable.Columns._

      val sql1 = sql"SELECT $name, $email, $other"
      val sql2 = fr"AND $other = 42"
      val sql3 = fr"$name ILIKE '${"admin"}'"
    }

    object AnotherUsersThing {
       import Tables.UsersTable.Columns._

       val sql1 = sql"SELECT $name, $email"
       val sql2 = fr"AND $other 0xDEADBEEF  "
    }

    // NOW think of having multiple tables that have columns with the same name,
    // and trying to find all the SQL bits referring to that column name--but
    // only for a specific table.  Trickier?  No!  Try traversing starting from
    // one of the following references to GroupsTable.Columns.name:

    object SomeGroupsThings {
      import Tables.GroupsTable.Columns._

      val sql1 = sql"SELECT $name, $something,"
      val sql2 = fr"$name ILIKE '${"admin"}'"
    }

  }
  TableSpecificClientCode

}
