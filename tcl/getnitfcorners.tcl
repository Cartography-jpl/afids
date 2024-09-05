#load ossimTcl.so

proc getNitfCorners {path} {

    set imageIndex 0

    set fields [ossim::getImageHeaderFields $path $imageIndex]

    return $fields
}

puts [getNitfCorners $argv]
