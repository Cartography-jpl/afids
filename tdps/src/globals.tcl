# vectorParts is an array mapping shapeVectorId (from a shapefile) to {canvasItemIds vectorPartOffsets}
# vectorData  is an array mapping canvasItemId to {shapeFilename shapeVectorId}
# loadedFiles is an array mapping shapeFilename to {vectorType vectorCount fieldDescriptionList canvasItemList shapeVectorIds changedFlag}

# ****************************************************************************************************************************

# vertexDotIds manipulators
# vertexDotIds is an array mapping vertex canvas id to vector canvas id
# see also global vectorVertexItems in vecEdit.tcl, mapping vector canvas item to vertex canvas item list
array set vertexDotIds {}

proc vertexIdToVertexList {vertex} {
    global vertexDotIds

    set vectorCanvasId $vertexDotIds($vertex)

    global vectorVertexItems

    return $vectorVertexItems($vectorCanvasId)
}

proc clearVertexDotIds {} {
    global vertexDotIds
    array unset vertexDotIds
}

proc mapVertexToVector {vertexId vectorId} {
    global vertexDotIds
    array set vertexDotIds [list $vertexId $vectorId]
}

proc unmapVertexToVector {vertexId} {
    global vertexDotIds
    array unset vertexDotIds($vertexId)
}

proc getVertexIds {} {
    global vertexDotIds
    array names vertexDotIds
}

proc vertexIdToVectorCanvasItem {vertex} {
    global vertexDotIds

    return $vertexDotIds($vertex)
}    
# these do the same thing
proc getVectorFromVertex {vertexCanvasId} {
    global vertexDotIds
    set vectorCanvasId $vertexDotIds($vertexCanvasId)

    return $vectorCanvasId
}

proc getVertexOffset {vertexCanvasId vectorCanvasId} {
    global vectorVertexItems

    set offset [lsearch $vectorVertexItems($vectorCanvasId) $vertexCanvasId]

    return $offset
}

# newVector manipulators
# newVectorItems is an array mapping filename to a list of canvas items
array set newVectorItems {}
# newVectorFiles is an array mapping canvas item to filename
array set newVectorFiles {}

proc addVector {canvasId filename} {
    global newVectorFiles
    global newVectorItems

    set newVectorFiles($canvasId) $filename
    lappend newVectorItems($filename) $canvasId

    setVectorData $canvasId $filename -1 $canvasId 0

    setLoadedFileChangedFlag $filename

    return $canvasId
}

proc removeAddedVector {canvasId} {
    global newVectorFiles
    global newVectorItems
    global editedFile

    set filename $editedFile

    array unset newVectorFiles($canvasId)

    set index [lsearch $newVectorItems($filename) $canvasId]
    set newItems [lreplace $newVectorItems($filename) $index $index]
    set newVectorItems($filename) $newItems

    unsetVectorData $canvasId
}

proc canvasIdToAddedVectorFilename {canvasId} {
    global newVectorFiles

    if {[lsearch -exact [array names newVectorFiles] $canvasId] < 0} {
	return ""
    } else {
	return $newVectorFiles($canvasId)
    }
}

proc getAddedVectorCanvasIds {filename} {
    global newVectorItems

    return $newVectorItems($filename)
}

proc deleteAddedVector {canvasId} {
    global newVectorFiles
    global newVectorItems

    if {[lsearch -exact [array names newVectorFiles] $canvasId] < 0} {
	return false
    } else {
	set filename $newVectorFiles($canvasId)
	unset newVectorFiles($canvasId)

	set canvasIds $newVectorItems($filename)
	set index [lsearch -exact $canvasIds $canvasId]
	set canvasIds [lreplace $canvasIds $index $index]
	set newVectorItems($filename) $canvasIds

	global c
	$c delete $canvasId

	return true
    }
}

proc initAddedVectorsForLoadedFile {filename} {
    global newVectorItems
    set newVectorItems($filename) {}
}

proc unloadAddedVectorsForLoadedFile {filename} {
    global newVectorItems
    unset newVectorItems($filename)
}

proc unloadAddedVectors {} {
    global newVectorItems
    array unset newVectorItems
    array set newVectorItems {}
}    

# this returns the vertex coordinates for all vector's parts
# this is used exclusively by the property viewer
proc canvasItemToVertices {item} {
    set canvasIds [canvasItemToCanvasParts $item]

    global c scalePower
    set vertices {}
    foreach id $canvasIds {
  	foreach coord [$c coords $id] {
  	    lappend vertices [expr $coord / pow (2, $scalePower)]
  	}
    }

    return $vertices
}

# vectorData manipulators
# vectorData is an array mapping canvasItem to {shapeFilename vectorId parts indices}
array set vectorData {}

proc setVectorData {canvasItem filename vectorId parts indices} {
    global vectorData

    array set vectorData [list $canvasItem [list $filename $vectorId $parts $indices]]
}

proc unsetVectorData {canvasItem} {
    global vectorData

    array unset vectorData($canvasItem)
}

proc canvasItemToFilename {item} {
    global vectorData

    set vecData [lindex [array get vectorData $item] 1]

    lindex $vecData 0
}

proc canvasItemToVectorId {item} {
    global vectorData

    set vecData [lindex [array get vectorData $item] 1]

    lindex $vecData 1
}

# all canvas ids associated including $item associated with $item's vector
proc canvasItemToCanvasParts {item} {
    global vectorData

    set vecData [lindex [array get vectorData $item] 1]

    lindex $vecData 2
}

# vertex offsets within vector
proc canvasItemToPartIndices {item} {
    global vectorData

    set vecData [lindex [array get vectorData $item] 1]

    lindex $vecData 3
}

proc clearVectorData {} {
    global vectorData

    array unset vectorData
}

# modifiedLoadedVectorsSHP manipulators
# modifiedLoadedVectorsSHP is an array mapping shapeFilename to a list of modified vectorIds
array set modifiedLoadedVectorsSHP {}
proc initModifiedVectorListForLoadedFile {filename} {
    global modifiedLoadedVectorsSHP

    set modifiedLoadedVectorsSHP($filename) {}
}

proc unloadModifiedVectorListForLoadedFile {filename} {
    global modifiedLoadedVectorsSHP

    unset modifiedLoadedVectorsSHP($filename)
}

proc unloadModifiedVectorList {} {
    global modifiedLoadedVectorsSHP

    array unset modifiedLoadedVectorsSHP
    array set modifiedLoadedVectorsSHP {}
}

proc recordModifiedVectorSHP {filename vectorId} {
    global modifiedLoadedVectorsSHP

    set modifiedVectors $modifiedLoadedVectorsSHP($filename)
    if {[lsearch -exact $modifiedVectors $vectorId] < 0} {
	lappend modifiedVectors $vectorId
	set modifiedLoadedVectorsSHP($filename) $modifiedVectors
	setLoadedFileChangedFlag $filename
    }
}

proc vectorModifiedSHP {filename vectorId} {
    global modifiedLoadedVectorsSHP

    if {[lsearch -exact $modifiedLoadedVectorsSHP($filename) $vectorId] < 0} {
	return false
    } else {
	return true
    }
}

# loadedFiles manipulators
# loadedFiles is an array mapping shapeFilename to {vectorType vectorCount fieldDescriptionList canvasItemList vectorIds firstItems changedFlag}
# firstItems is a list of the first canvas id for each vector. This is used to find all the canvas ids for a vector.
array set loadedFiles {}

proc setLoadedFile {filename vectorType vectorCount fieldDescriptions items vecIds firstItems changedFlag} {
    global loadedFiles

    array set loadedFiles [list $filename [list $vectorType $vectorCount $fieldDescriptions $items $vecIds $firstItems $changedFlag]]
}

proc unsetLoadedFile {filename} {
    global loadedFiles
    
    unset loadedFiles($filename)
}

proc clearLoadedFiles {} {
    global loadedFiles
    
    array unset loadedFiles
}

# get all loaded files
proc getLoadedFilenames {} {
    global loadedFiles

    array names loadedFiles
}

# get all loaded file data
proc getLoadedFileData {name} {
    global loadedFiles

    lindex [array get loadedFiles $name] 1
}

# get vectorType
proc getLoadedFileVectorType {name} {
    lindex [getLoadedFileData $name] 0
}

# get vectorCount
proc getLoadedFileVectorCount {name} {
    lindex [getLoadedFileData $name] 1
}

# get fieldDescriptionList
proc getLoadedFileFieldDescriptions {name} {
    lindex [getLoadedFileData $name] 2
}

# get canvasItemList
proc getLoadedFileVectorCanvasItems {name} {
    lindex [getLoadedFileData $name] 3
}

proc getLoadedFileCanvasItems {name} {
    eval concat [getLoadedFileVectorCanvasItems $name]
}

# get vectorIds
proc getLoadedFileVectorIds {name} {
    lindex [getLoadedFileData $name] 4
}

proc getLoadedFileFirstItems {name} {
    lindex [getLoadedFileData $name] 5
}

# changed flag
proc getLoadedFileChangedFlag {name} {
    lindex [getLoadedFileData $name] 6
}

# setLoadedFileChangedFlag $filename
#  addVector
#   splitVector "Break Object (C-B)"
#   scaleAndRestoreVector
#    captureUndoAction
#   pasteVectorPasteBuffer "Paste (C-V)"
#   addLineAt "button1Press"
#   importLineVectors1 "Line vectors ..."
#   importPolygonVectors1 "Polygon vectors ..."
#   addPolygonAt "button1Press"
#  recordModifiedVectorSHP
#   saveModifiedVectorCanvasItem
#    deleteVertices "Delete Vertices"
#    insertVertices "Insert Vertices (C-P)"
#    saveModifiedSelection
#     placeAndSelectNextVertex button3Press
#     nudgeVector bindings and menus
#     button1Release
#  cutSelectedVectorsInternal
#   cutSelectedVectors "Cut (C-X)"
#   deleteSelectedVectors "Delete (Del)"
#    deleteBackspace (menu, canvas binding on del, bspc)
proc setLoadedFileChangedFlag {name} {
    set loadedFilenames [getLoadedFilenames]
    if {[lsearch $loadedFilenames $name] < 0} {
	return
    }

    set data [getLoadedFileData $name]

    set data [lreplace $data 6 6 1]

    global loadedFiles
    set loadedFiles($name) $data
}

proc clearLoadedFileChangedFlag {name} {
    set loadedFilenames [getLoadedFilenames]
    if {[lsearch $loadedFilenames $name] < 0} {
	return
    }

    initModifiedVectorListForLoadedFile $name

    set data [getLoadedFileData $name]

    set data [lreplace $data 6 6 0]

    global loadedFiles
    set loadedFiles($name) $data
}

proc vectorIdToCanvasFirstItem {vecId filename} {
    set vectorIds [getLoadedFileVectorIds $filename]
    set index [lsearch -exact $vectorIds $vecId]
    set firstItems [getLoadedFileFirstItems $filename]
    lindex $firstItems $index
}

proc vectorIdToCanvasParts {vecId filename} {
    set firstItem [vectorIdToCanvasFirstItem $vecId $filename]

    canvasItemToCanvasParts $firstItem
}

proc deleteCanvasItem {canvasId} {
    # if was is an added vector, don't add it
    if {[deleteAddedVector $canvasId]} {
	return
    }

    set canvasIds [canvasItemToCanvasParts $canvasId]

    set filename [canvasItemToFilename $canvasId]

    set vectorType [getLoadedFileVectorType $filename]
    set vectorCount [getLoadedFileVectorCount $filename]
    set fieldDescriptions [getLoadedFileFieldDescriptions $filename]
    set canvasItems [getLoadedFileCanvasItems $filename]
    set vectorIds [getLoadedFileVectorIds $filename]
    set firstItems [getLoadedFileFirstItems $filename]

    if {[lsearch $canvasItems $canvasId] < 0} {
	return
    }

    set canvasIdCount [llength $canvasIds]

    foreach canvasId $canvasIds {
	# remove canvas item
	set index [lsearch $canvasItems $canvasId]
	set canvasItems [lreplace $canvasItems $index $index]

	global c
	$c delete $canvasId
    }

    # remove vector id
    set vecId [canvasItemToVectorId $canvasId]
    set index [lsearch $vectorIds $vecId]
    set vectorIds [lreplace $vectorIds $index $index]
    set firstItems [lreplace $firstItems $index $index]

    # update loaded file data
    setLoadedFile $filename $vectorType $vectorCount $fieldDescriptions $canvasItems $vectorIds $firstItems 1
}

proc deleteVectorIgnoringCanvasItems {canvasId} {
    # if was is an added vector, don't add it
    if {[deleteAddedVector $canvasId]} {
	return
    }

    set filename [canvasItemToFilename $canvasId]

    set vectorType [getLoadedFileVectorType $filename]
    set vectorCount [getLoadedFileVectorCount $filename]
    set fieldDescriptions [getLoadedFileFieldDescriptions $filename]
    set canvasItems [getLoadedFileCanvasItems $filename]
    set vectorIds [getLoadedFileVectorIds $filename]
    set firstItems [getLoadedFileFirstItems $filename]

    if {[lsearch $canvasItems $canvasId] < 0} {
	return
    }

    # remove vector id
    set vecId [canvasItemToVectorId $canvasId]
    set index [lsearch $vectorIds $vecId]
    set vectorIds [lreplace $vectorIds $index $index]
    set firstItems [lreplace $firstItems $index $index]

    # update loaded file data
    setLoadedFile $filename $vectorType $vectorCount $fieldDescriptions $canvasItems $vectorIds $firstItems 1
}

