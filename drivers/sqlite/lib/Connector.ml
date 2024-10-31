module T : DBCaml.CONNECTOR = struct
  let connect conninfo =
    DBCaml.Driver.Driver
      { driver = (module Driver); config = Driver.{ conninfo } }
end

include T
