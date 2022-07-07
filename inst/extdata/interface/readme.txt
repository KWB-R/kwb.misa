parameter_conversion.xlsx
  table used by r-script "infoworks_gerris.R" for the conversion of parameter IDs and
  definition of parameters not considered in infoworks but needed for gerris (waterquality)
  Columns:
    parameter: full German Parameter Name
    id_infoworks: parameter ID in Infoworks (mf = mass flow)
    unit_infoworks: unit of the infoworks output (mass flow)
    constant_value: parameters that are not simulated by infoworks and are set to a constant value
      unit is the same as in gerris (see column "unit_gerris")
    id_gerris: parameter ID in Gerris
    unit_gerris: unit needed for Gerris input (concentration)
    min_gerris and max_gerris: tolerated range of parameter in Gerris model
    comment: possibility to add comments (column not used by r-script)

outlet_conversion.xlsx 
  table used by r-script "infoworks_gerris.R" for the conversion of outlet IDs
  Columns:
    outlet_id: Node ID of the outlet within the infoworks model
    upstream_link_id: Infoworks ID of the link leading to outlet (this must be used
      for mass flow information)
    gerris_id: "Randbedingung" used in gerris (provided by Schumacher, Ingenieurbüro 
      für Wasser und Umwelt)
    surface_water: name of the surface water the outlet discharges into
    water_body_km: the water body km where the outlet is located
    y_coordinate and x_coordanite: outlet location in soldner coordinates for Berlin
      (Infoworks export)
    all lat and lon columns: coordinates transformed from Soldner to UTM-Format (ETRS89 DREF91)
    comment: possibility to add comments (column not used by r-script)  

    
    
   

