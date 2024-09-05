package provide dbtcl 1.0

##############################################################################
#   COMMAND         : Data_Delete
#   DESCRIPTION     : This command deletes the rows of a table which satisfy
#                     all criteria in the criteria list. If the criteria list
#                     is the empty list, all rows are deleted. Each element of
#                     the criteria list is a two item list. The first item is
#                     a valid COLUMN_NAME for the table. The second item is
#                     either a string value which must be matched precisely
#                     or a two element list indicating a minimum and maximum
#                     numerical value for matching.
#   PARAMETERS      : table    - the table name
#                     criteria - the list of {COLUMN_NAME criteria} lists
#   RETURNS         : none
##############################################################################
proc Data_Delete {table criteria} {
    global env

    set data {}
    set format [Data_Format ${table}]

    set file [open $env(AFIDS_DATA)/tma/${table}.dat r]
    while {[gets ${file} record] >= 0} {
        if {[Data_Check ${record} ${format} ${criteria}] != {}} {
            lappend data ${record}
        }
    }
    close ${file}

    Data_Dump ${table} ${data}
}

##############################################################################
#   COMMAND         : Data_Insert
#   DESCRIPTION     : This command inserts a row into a table using the value
#                     specifications in the values list. Each element of the
#                     values list is a two item list. The first item is a
#                     valid COLUMN_NAME for the table. The second item is a
#                     string value to be set for that column in the new row.
#                     Any column not set explicitly in the values list is set
#                     to the empty list by default.
#   PARAMETERS      : table    - the table name
#                     values   - the list of {COLUMN_NAME value} lists
#   RETURNS         : none
##############################################################################
proc Data_Insert {table values} {
    global env

    set format [Data_Format ${table}]
    foreach column ${format} {
        lappend record {}
    }

    foreach value ${values} {
        set index [lsearch ${format} [lindex ${value} 0]]
        set record [lreplace ${record} ${index} ${index} [lindex ${value} 1]]
    }

    set file [open $env(AFIDS_DATA)/tma/${table}.dat a]
    puts ${file} ${record}
    close ${file}
}

##############################################################################
#   COMMAND         : Data_Select
#   DESCRIPTION     : This command selects values for columns in the columns
#                     list from the rows of a table which satisfy all criteria
#                     in the criteria list. If the columns list is the empty
#                     list, all columns are selected. If the criteria list is
#                     the empty list, all rows are selected. Each element of
#                     the criteria list is itself a two item list. The first
#                     item is a valid COLUMN_NAME for the table. The second
#                     item is either a string value which must be matched
#                     precisely or a two element list indicating a minimum
#                     and maximum numerical value for matching.
#   PARAMETERS      : table    - the table name
#                     columns  - the list of COLUMN_NAME values to select
#                     criteria - the list of {COLUMN_NAME criteria} lists
#   RETURNS         : a list of rows where each row is a list of column values
##############################################################################
proc Data_Select {table columns criteria} {
    global env

    set data {}
    set format [Data_Format ${table}]

    set file [open $env(AFIDS_DATA)/tma/${table}.dat r]
    while {[gets ${file} record] >= 0} {
        if {[Data_Check ${record} ${format} ${criteria}] == {}} {
            if {${columns} == {}} {
                set view ${record}
            } else {
                set view {}
                foreach column ${columns} {
                    set index [lsearch ${format} ${column}]
                    set item [lindex ${record} ${index}]
                    lappend view ${item}
                }
            }
            lappend data ${view}
        }
    }
    close ${file}

    set data
}

##############################################################################
#   COMMAND         : Data_Update
#   DESCRIPTION     : This command updates the rows of a table which satisfy
#                     all criteria in the list using the value specifications
#                     in the values list. If the criteria list is the empty
#                     list, all rows are updated. Each element of the criteria
#                     list is a two item list. The first item is a valid
#                     COLUMN_NAME for the table. The second item is either
#                     a string value which must be matched precisely or a two
#                     element list indicating a minimum and maximum numerical
#                     value for matching. Each element of the values list is
#                     a two item list. The first item is a valid COLUMN_NAME
#                     for the table. The second item is a string value to be
#                     set for that column in the row.
#   PARAMETERS      : table    - the table name
#                     values   - the list of {COLUMN_NAME value} lists
#                     criteria - the list of {COLUMN_NAME criteria} lists
#   RETURNS         : none
##############################################################################
proc Data_Update {table values criteria} {
    global env

    set format [Data_Format ${table}]

    set file [open $env(AFIDS_DATA)/tma/${table}.dat r]
    while {[gets ${file} record] >= 0} {
        if {[Data_Check ${record} ${format} ${criteria}] == {}} {
            foreach value ${values} {
                set index [lsearch ${format} [lindex ${value} 0]]
                set record [lreplace ${record} ${index} ${index} [lindex ${value} 1]]
            }
        }
        lappend data ${record}
    }
    close ${file}

    Data_Dump ${table} ${data}
}

##############################################################################
#                                                                            #
#                              PRIVATE COMMANDS                              #
#                                                                            #
##############################################################################

##############################################################################
#   COMMAND         : Data_Check
#   DESCRIPTION     : This command 
#   PARAMETERS      : record   - the data record
#                     format   - the format of the table
#                     criteria - the list of {COLUMN_NAME criteria} lists
#   RETURNS         : the list of columns not satisfying the criteria
##############################################################################
proc Data_Check {record format criteria} {
    set badcolumns {}
    foreach crit ${criteria} {
        set column [lindex ${crit} 0]
        set value  [lindex ${crit} 1]
        set index  [lsearch ${format} ${column}]
        set item   [lindex ${record} ${index}]
        if {[string match ${value} ${item}] == 0} {
        #    if {[llength ${value}] == 2 && 
        #        [scan [lindex ${value} 0] "%f" lower] &&
        #        [scan [lindex ${value} 1] "%f" upper] &&
        #        ${lower} < ${upper}} {
        #        if {${item} < ${lower} ||
        #            ${item} > ${upper}} {
        #            lappend badcolumns ${column}
        #        }
        #    } else {
                lappend badcolumns ${column}
        #    }
        }
    }

    set badcolumns
}

##############################################################################
#   COMMAND         : Data_Dump
#   DESCRIPTION     : This command rebuilds a table using the rows in data.
#   PARAMETERS      : table    - the table name
#                     data     - the rows for the table
#   RETURNS         : none
##############################################################################
proc Data_Dump {table data} {
    global env

    set file [open $env(AFIDS_DATA)/tma/${table}.dat w]
    foreach record ${data} {
        puts ${file} ${record}
    }
    close ${file}
}

##############################################################################
#   COMMAND         : Data_Exit
#   DESCRIPTION     : This command cleans up the environment and terminates
#                     the Data server process.
#   PARAMETERS      : none
#   RETURNS         : none
##############################################################################
#proc Data_Exit {} {
#    Comm_ServerRelease Data
#    exit
#}

##############################################################################
#   COMMAND         : Data_Format
#   DESCRIPTION     : This command retrieves the format of a table.
#   PARAMETERS      : table    - the table name
#   RETURNS         : the list of COLUMN_NAME titles for the table
##############################################################################
proc Data_Format {table} {
    global env

    set file [open $env(AFIDS_DATA)/tma/${table}.fmt r]
    while {[gets ${file} column] >= 0} {
        lappend format $column
    }
    close ${file}

	set format
}

##############################################################################
#   COMMAND         : Data_Init
#   DESCRIPTION     : This command sets up the environment and initiates
#                     the Data server process.
#   PARAMETERS      : none
#   RETURNS         : none
##############################################################################
#proc Data_Init {} {
#    package require Comm
#    Comm_ServerRegister Data
#
#    wm title . Data
#    button .b -text {Kill Data Server} -command Data_Exit
#    pack .b -fill both -expand true
#}

