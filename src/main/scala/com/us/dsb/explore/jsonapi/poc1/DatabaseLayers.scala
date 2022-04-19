package com.us.dsb.explore.jsonapi.poc1

/** Generic database types. */
object Database {
  case class TableName( raw: String) extends AnyVal
  case class ColumnName(raw: String) extends AnyVal
  case class RowKey(    raw: String) extends AnyVal // key value, not key column name
  //?? Any need to model different column data types?
}
import Database._

/** Generic database actions. */
trait Database {
  /** Like SELECT via (scalar) primary key, for specified columns. */
  def selectSpecificRow(tableName: TableName,
                        rowKey: RowKey,
                        columnNames: ColumnName*
                       ): Option[Map[ColumnName, Any]]
  def selectAllRows(tableName: TableName,
                    columnNames: ColumnName*
                   ): Seq[Map[ColumnName, Any]]
  //?? Q: Prototype what other "shapes" of queries?
  // - ???? Q: HEY, what query(-ies) would populate a list of user entities
  //   _each_ of which lists related groups?  (A JOIN to get related groups
  //   could list multiple rows for each user.  (Maybe no columns; maybe many.)
  //   (Even more duplication if multiple relationships "done" in single query.))
  //   Would we use one query for set of users, plus one query for each
  //   relationship? (Get filtered set of users with selected columns; then
  //   ???: ... filter users, join to related things, whichever columns of related
  //   things ... .
  //   Q: Re-check multi-hop relationships:  What does JSON:API allow/constrain?
  //   What will we support?
  //   Q: Note ~difference between one user's related groups vs. list of users
  //   all listing related groups.  (What did I mean?)
  //   Q: Would UI showing list page want to already have listed groups, or
  //   can it get list for a selected item just get it on demand?
}

/** Specific database/schema declaration and private implementation. */
object DatabaseImpl extends Database {
  object TableNames {
    val users:   TableName = TableName("users_table")
    val domains: TableName = TableName("domains_table")
  }

  object UserColumnNames {
    val object_guid: ColumnName = ColumnName("object_guid")
    val user_name:   ColumnName = ColumnName("user_name")
    val domain_name: ColumnName = ColumnName("domain_name")
    /** (assuming: to-1 relationship; FK to domain tables PK */
    val domain_fk:   ColumnName = ColumnName("domain_fk")  //???? rel. metadata must identity FK and target PK
    val some_int:    ColumnName = ColumnName("some_int")
    val some_enum:   ColumnName = ColumnName("some_enum")
  }

  object DomainColumnNames {
    val object_guid: ColumnName = ColumnName("object_guid")
    val domain_name: ColumnName = ColumnName("domain_name")
    val domain_enum: ColumnName = ColumnName("domain_enum")
  }

  private val usersTable = {
    import UserColumnNames._
    Map(
      "user0123-fake-guid" -> Map[ColumnName, Any](
        object_guid -> "user0123-fake-guid",
        user_name -> "User 123",
        domain_name -> "Domain 1",
        domain_fk -> "domain01-fake-guid",
        some_int -> 1,
        some_enum -> "One"
        ),
      "user0456-fake-guid" -> Map[ColumnName, Any](
        object_guid -> "user0456-fake-guid",
        user_name -> "User 456",
        domain_name -> "Domain 1",
        domain_fk -> "domain01-fake-guid",
        some_int -> 2,
        some_enum -> "BogUs"
        )
      )
  }
  private val domainsTable = {
    import DomainColumnNames._
    Map(
      "domain01-fake-guid" -> Map[ColumnName, Any](
        object_guid -> "user0123-fake-guid",
        domain_name -> "Domain 1",
        domain_enum -> "Dough"
        )
      )
  }

  private val tables =
    Map(TableNames.users   -> usersTable,
        TableNames.domains -> domainsTable
        )

  import Database._

  def selectSpecificRow(tableName: TableName,
                        rowKey: RowKey,
                        columnNames: ColumnName*
                       ): Option[Map[ColumnName, Any]] = {
    //println(s"selectSpecificRow.1: tableName = $tableName, rowKey = $rowKey, columnNames = ${columnNames}")
    val tableFullRows = tables(tableName)
    val fullSelectedRowOpt = tableFullRows.get(rowKey.raw)  //?? .get

    val row = {
      fullSelectedRowOpt.map { allColumns =>
        //println(s"selectSpecificRow.2: allColumns = ${allColumns}")
        val selectedColumns: Map[ColumnName, Any] =
          columnNames.map { columnName =>
            //println("selectSpecificRow.3:   columnName = " + columnName)
            columnName -> allColumns(columnName)
          }.toMap
        //println("selectSpecificRow.4:   selectedColumns = " + selectedColumns)
        selectedColumns
      }
    }
    row


  }



  override def selectAllRows(tableName  : TableName,
                             columnNames: ColumnName*
                            ): Seq[Map[ColumnName, Any]] = {
    //println(s"selectAllRows.1: tableName = $tableName, columnNames = ${columnNames}")
    val tableFullRows = tables(tableName)
    val rows = {
      tableFullRows.map { case (key, allColumns) =>
        //println(s"selectAllRows.2: key = $key, allColumns = ${allColumns}")
        val selectedColumns: Map[ColumnName, Any] =
          columnNames.map { columnName =>
            //println("selectAllRows.3:   columnName = " + columnName)
            columnName -> allColumns(columnName)
          }.toMap
        //println("selectAllRows.4:   selectedColumns = " + selectedColumns)
        selectedColumns
      }
    }
    rows.toSeq
  }
}

