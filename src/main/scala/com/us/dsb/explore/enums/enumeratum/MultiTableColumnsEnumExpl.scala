package com.us.dsb.explore.enums.enumeratum

import doobie.syntax.SqlInterpolator.SingleFragment
import enumeratum.{Enum, EnumEntry}

import scala.util.chaining.scalaUtilChainingOps


/** Exploration of common things on enumerations of table columns. */
object MultiTableColumnsEnumExpl extends App {

  // Note: Trying semantically accurate different "column" vs. "columns" on
  //   things that may typically be named the same (instances of "column" vs. lists
  //   of columns.

  object ImplementationIndependent {  // (Some of the would be packages.)

    /** Table column.  (Not necessarily implemented via `Enum`/`EnumEntry`.) */
    trait BaseTableColumn {
      /**
       * Gets simple (unqualified) SQL name of this column.
       */
      def toSqlSimpleName: String

    }

    object BaseTableColumn {

      // NOTE: The following implicit conversion could be moved from
      // BaseTableColumn's companion object to other object from where it could
      // be imported only explicitly.

      import scala.language.implicitConversions
      import doobie.Fragment
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

    /** Table column implemented with Enumeratum. */
    trait EnumTableColumn extends BaseTableColumn with EnumEntry {

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
      /** A column in the users table. */
      sealed trait Column extends EnumTableColumn with EnumEntry

      /** Declares the columns in the users table. */
      object Columns extends EnumTableColumnsList[Column] {

        case object name       extends Column with NameColumn
        case object user_email extends Column with TextSearchableColumn
        case object user_other extends Column
        // Big sequence of other columns goes here.

        override val values = findValues
      }
      override val getColumns = Columns
    }

    object GroupsTable extends BaseTable("groups") {
      sealed trait Column extends EnumTableColumn with EnumEntry

      object Columns extends EnumTableColumnsList[Column] {

        case object name            extends Column with NameColumn
        case object group_something extends Column
        //case object abnormal        extends GroupsColumn with NameColumn
        // Big sequence of other columns goes here.

        override val values = findValues
      }
      override val getColumns = Columns
    }

    object OtrosTabla extends BaseTable("otras_cosas") {
      sealed trait TablaColumna extends EnumTableColumn with EnumEntry

      object Columnas extends EnumTableColumnsList[TablaColumna] {

        case object nombre        extends TablaColumna with NameColumn
        case object cosa_de_texto extends TablaColumna with TextSearchableColumn
        case object otra_cosa     extends TablaColumna
        // Big sequence of other columns goes here.

        override val values = findValues
      }
      override val getColumns = Columnas
    }
  }

  object Clients {
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
                 UsersTable.Columns.user_email,
                 UsersTable.Columns.user_other
                 ))

    println("UsersTable.Columns.getSearchColumns  = " + UsersTable.Columns.getSearchColumns)
    assert(UsersTable.Columns.getSearchColumns ==
               Vector(
                 UsersTable.Columns.name,
                 UsersTable.Columns.user_email
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
    println("""fr"$name ILIKE '$value1'" = """ + fr"$name ILIKE '$value'" )
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
    assert( makeSearchSql("findme", usersTableGenerically.getColumns).toString ==
      """Fragment("(name ILIKE '?' ) OR (user_email ILIKE '?' ) ")""")

  }
  Clients

}

