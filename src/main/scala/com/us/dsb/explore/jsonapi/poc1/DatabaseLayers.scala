package com.us.dsb.explore.jsonapi.poc1


/** Generic database types. */
object Database {
  case class RowKey(raw: String) extends AnyVal
  case class TableName(raw: String) extends AnyVal
  case class ColumnName(raw: String) extends AnyVal
}
import Database._

/** Generic database actions. */
trait Database {
  def selectSpecificRow(tableName: TableName, rowKey: RowKey, columnNames: ColumnName*): Option[Map[ColumnName, Any]]
  def selectAllRows(tableName: TableName, columnNames: ColumnName*): Seq[Map[ColumnName, Any]]
}

/** Specific database/schema declaration and private implementation. */
object DatabaseImpl extends Database {
  object TableNames {
    val users: TableName = TableName("users_table")
  }

  object UserColumnNames {
    val object_guid: ColumnName = ColumnName("object_guid")
    val user_name: ColumnName = ColumnName("user_name")
    val some_int: ColumnName = ColumnName("some_int")
  }

  private val usersTable = {
    import UserColumnNames._
    Map(
      "user0123-fake-guid" -> Map[ColumnName, Any](
        object_guid -> "user0123-fake-guid",
        user_name -> "User 123",
        some_int -> 1
        ),
      "user0456-fake-guid" -> Map[ColumnName, Any](
        object_guid -> "user0456-fake-guid",
        user_name -> "User 456",
        some_int -> 2
        )
      )
  }

  private val tables =
    Map(TableNames.users -> usersTable
        )

  import Database._

  def selectSpecificRow(tableName: TableName,
                        rowKey: RowKey,
                        columnNames: ColumnName*
                       ): Option[Map[ColumnName, Any]] = {
    println(s"selectSpecificRow.1: tableName = $tableName, rowKey = $rowKey, columnNames = ${columnNames}")
    val tableFullRows = tables(tableName)
    val fullSelectedRowOpt = tableFullRows.get(rowKey.raw)  //?? .get

    val row = {
      fullSelectedRowOpt.map { allColumns =>
        println(s"selectSpecificRow.2: allColumns = ${allColumns}")
        val selectedColumns: Map[ColumnName, Any] =
          columnNames.map { columnName =>
            println("selectSpecificRow.3:   columnName = " + columnName)
            columnName -> allColumns(columnName)
          }.toMap
        println("selectSpecificRow.4:   selectedColumns = " + selectedColumns)
        selectedColumns
      }
    }
    row


  }



  override def selectAllRows(tableName  : TableName,
                             columnNames: ColumnName*
                            ): Seq[Map[ColumnName, Any]] = {
    println(s"selectAllRows.1: tableName = $tableName, columnNames = ${columnNames}")
    val tableFullRows = tables(tableName)
    val rows = {
      tableFullRows.map { case (key, allColumns) =>
        println(s"selectAllRows.2: key = $key, allColumns = ${allColumns}")
        val selectedColumns: Map[ColumnName, Any] =
          columnNames.map { columnName =>
            println("selectAllRows.3:   columnName = " + columnName)
            columnName -> allColumns(columnName)
          }.toMap
        println("selectAllRows.4:   selectedColumns = " + selectedColumns)
        selectedColumns
      }
    }
    rows.toSeq
  }
}

