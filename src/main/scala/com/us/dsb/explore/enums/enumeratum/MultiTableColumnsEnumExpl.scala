package com.us.dsb.explore.enums.enumeratum

import enumeratum.{Enum, EnumEntry}

import scala.util.chaining.scalaUtilChainingOps


/** Exploration of common things on enumerations of table columns. */
object MultiTableColumnsEnumExpl extends App {

  // Note: Trying semantically accurate different "column" vs. "columns" on
  //   things that may typically be named the same (instances of "column" vs. lists
  //   of columns.

  object ImplementationIndependent {

    /** Table column.  (Not necessarily implemented via `Enum`/`EnumEntry`.) */
    trait BaseTableColumn {
      def toSqlSimpleName: String
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


    /** List of columns for a table.
     * (Not necessarily named `name`; not necessarily implemented via `Enum`/`EnumEntry`.)
     */
    trait TableColumnsList {

      // Some application-specific special characteristics/properties of tables:

      /** Gets this column set's logical name column. */
      def getNameColumn: NameColumn

      /** Gets this column set's columns to be text-searched. */
      val getSearchColumns: IndexedSeq[TextSearchableColumn]
    }

  }

  object EnumImplementation {
    import ImplementationIndependent._

    /** Table column implemented with Enumeratum. */
    trait EnumTableColumn extends BaseTableColumn with EnumEntry {

      /**
       * Gets simple (unqualified) SQL name of this column.
       *
       * This implementation delegates to `entryName`.  Note that that works
       * only if that value is a valid regular SQL identifer.  Otherwise, the
       * column needs to override this method. (Or this need to be re-written to
       * recognize and properly quote irregular identifiers.)
       */
      def toSqlSimpleName: String = entryName
    }

    /** List of columns for a table, implemented with Enumeratum. */
    trait EnumTableColumnsList[TC <: EnumEntry] extends TableColumnsList with Enum[TC] {

      /** Gets this column set's chosen/designated name column. */
      override lazy val getNameColumn: NameColumn = {
        values.flatMap { col =>
          col match {
            case nameColumn: NameColumn => Some(nameColumn)
            case _ => None
          }
        }
        .tap { found =>
          assert( found.size <= 1, s"Multiple NameColumn columns found: ${found.mkString(", ")}.")
        }
        .headOption.getOrElse(throw new NoSuchElementException("No NameColumn column found."))

        // Note:  Can't check for having multiple name columns at initialization
        // time, because this getNameColumn has to be lazy to not execute before
        // "values" is initialized (later, since on subclass of this trait?).
        // (Removing "lazy" above leads to NullPointerException.)
      }

      /** Gets this column set's ~tagged searchable text columns. */
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
    import EnumImplementation.EnumTableColumn
    import EnumImplementation.EnumTableColumnsList


    object UsersTable {
      /** A column in the users table. */
      sealed trait UsersTableColumn extends EnumTableColumn with EnumEntry

      /** Lists the columns in the users table. */
      object UsersTableColumns extends EnumTableColumnsList[UsersTableColumn] {

        case object name extends UsersTableColumn       with NameColumn
        case object user_email extends UsersTableColumn with TextSearchableColumn
        case object user_other extends UsersTableColumn

        override val values = findValues
      }
    }

    object GroupsTable {
      sealed trait GroupsTableColumn extends EnumTableColumn with EnumEntry

      object GroupsTableColumns extends EnumTableColumnsList[GroupsTableColumn] {

        case object name            extends GroupsTableColumn with NameColumn
        case object group_something extends GroupsTableColumn
        //case object abnormal        extends GroupsTableColumn with NameColumn

        override val values = findValues
      }
    }

    object OtrosTabla {
      sealed trait OtrosTablaColumna extends EnumTableColumn with EnumEntry

      object OtrosTablaColumnas extends EnumTableColumnsList[OtrosTablaColumna] {

        case object nombre    extends OtrosTablaColumna with NameColumn
        case object otra_cosa extends OtrosTablaColumna
        override val values = findValues
      }
    }
  }

  object Clients {
    import ImplementationIndependent.NameColumn
    import ImplementationIndependent.TableColumnsList

    import Tables.UsersTable._
    import Tables.GroupsTable._
    import Tables.OtrosTabla._



    println("UsersTableColumns.values = " + UsersTableColumns.values)
    println("GroupsTableColumns.values = " + GroupsTableColumns.values)

    UsersTableColumns.name: UsersTableColumn
    //UsersTableColumns.name : GroupColumn
    UsersTableColumns.name: NameColumn

    //GroupsTableColumns.name : UserColumn
    GroupsTableColumns.name: GroupsTableColumn
    GroupsTableColumns.name: NameColumn


    UsersTableColumns.name.toSqlSimpleName
    UsersTableColumns.user_email.toSqlSimpleName


    GroupsTableColumns
    println("GroupsTableColumn.getSearchColumns = " + GroupsTableColumns.getSearchColumns)
    println("GroupsTableColumn.getNameColumn    = " + GroupsTableColumns.getNameColumn)


    println("UsersTableColumns.name             = " + UsersTableColumns.name)
    println("UsersTableColumns.getNameColumn    = " + UsersTableColumns.getNameColumn)
    println("UsersTableColumns.getSearchColumns = " + UsersTableColumns.getSearchColumns)
    //println("OtrosTableColumns.name             = " + OtrosTableColumns.name)
    println("OtrosTablaColumna.getNameColumn    = " + OtrosTablaColumnas.getNameColumn)
    println("OtrosTablaColumna.getSearchColumns = " + OtrosTablaColumnas.getSearchColumns)

    val genericColumnsList: TableColumnsList = UsersTableColumns
    //println("genericColumnsList.name = " + genericColumnsList.name)
    println("genericColumnsList.getNameColumn = " + genericColumnsList.getNameColumn)
    println("genericColumnsList.getSearchColumns = " + genericColumnsList.getSearchColumns)


    //  def makeSearchSql[T](tableCols:  TableColumnsSomething[T]): {
    //
    //  }



    //??? show Doobie fr/sql use (via implicit conversion)

  }
  Clients

}
