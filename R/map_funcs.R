#####################################################################
# Check_map function
#####################################################################
#' Checks whether the desired map background is available.
#' 
#' The \code{check_map} queries the map database to check for available maps.
#' The parameters taken into account are the bounding box (extent of the spatial object)
#' and the desired scale of the map. 
#'
#' @param db 	    a SQLiteConnection object that specifies the connection to the database.
#' @param ext_map   a spatial object of class 'extent' from the 'raster' package
#'				    holding the data on the extreme coordinates of the input object. 
#' @param options_list a list of graphical and other parameters used by the rendering engine.
#' @return mapl     a data frame holding one record from the map database.
#' @keywords internal

check_map <- function(db, ext_map, options_list = NULL) 
{
  mapl <- RSQLite::dbGetQuery(db, paste0("SELECT * FROM Maplist WHERE bbox1<='", ext_map[1],
                                "' AND bbox2<='", ext_map[3],
                                "' AND bbox3>='", ext_map[2],
                                "' AND bbox4>='", ext_map[4],
                                "' AND SCALE ='", options_list$mscale,"'"))    
  
  if (nrow(mapl) > 1){
    x <- readline("Which map to choose (enter the desired map's ID)?")    
    mapl <- mapl[mapl$ID==x]
  }
  if (nrow(mapl) == 0) {
    mapl <- FALSE
  }
  return(mapl)
}

#####################################################################
# Render_map function
#####################################################################
#' Renders required map background using OSM data and mapnik rendering engine.
#' 
#' The \code{render_map} passes the bounding box and the graphical parameters of the 
#' required map background to the mapnik rendering engine. The map is rendered by mapnik
#' based on OpenStreetMap data stored in a local PostgreSQL database. 
#' @param map_data  a character string with the path to the folder storing the map data.
#' @param db 	    a SQLiteConnection object that specifies the connection to the database.
#' @param ext_map   a spatial object of class 'extent' from the 'raster' package
#'				    holding the data on the extreme coordinates of the input object. 
#' @param options_list a list of graphical and other parameters used by the rendering engine.
#' @return NULL 	there are no values returned by this function to the workspace; the output
#'					of the function is just the map file written in the /ttemp folder.  
#' @keywords internal

render_map <- function (map_data, ext_map, options_list = NULL, db) 
{ 
  def_options  <-  list(mscale = 50000,
                        url = NULL,
                        wld = F,
                        dpi = 150,
                        paper = NULL,
                        margin = 0.05,
                        size = NULL,
                        xmstyle = "C:/GIS_data/osm_mapnik/gis.xml",
                        output  = "map.tif",
                        tiles = F,
                        just_tiles = F,
                        force_rend = FALSE)
  
  # Checking if specified options are correct
  if (!is.null(options_list)){
    fitting_options <- which(names(options_list) %in% names(def_options))
    if (length(fitting_options) < length(options_list)){
      warning('Unrecognised options were specified')
    }
    
    # Overwriting default options
    mo <- match(names(options_list[fitting_options]), names(def_options))
    def_options[mo] <- options_list[fitting_options]
  }
  
  #Setting the extent of the map including margin
  b_box = paste(ext_map[1], ext_map[3], ext_map[2], ext_map[4], collapse = " ")
  
  # Calling the rendering engine 
  shell(paste0("nik4.py --bbox ", b_box ,
               " --scale ", def_options$mscale," --dpi ", def_options$dpi," ",
               ifelse(def_options$url, paste0(" --url ", def_options$url), " "),
               ifelse(def_options$size, paste0(" --size ", def_options$size), " "),
               ifelse(def_options$paper, paste0(" --paper ", def_options$paper), " "),
               ifelse(def_options$wld, paste0(" --wld ", def_options$wld), " "),
               def_options$xmstyle," ",
               paste0(map_data,"/ttemp/",def_options$output)," ",
               ifelse(def_options$tiles, paste0(" --tiles ",def_options$tiles), " "),
               ifelse(def_options$just_tiles, paste0(" --just-tiles "),"")) )
}

#################################################################
# Save_map function
#################################################################
#' Save the map file to a proper subdirectory and update the database. 
#' 
#' The \code{save_map} function renames and copies the map file to the 
#' appropriate directory after rendering and updates the database with 
#' the map attributes.
#'
#' @param map_data  a character string with the path to the folder storing the map data.
#' @param db 	    a SQLiteConnection object that specifies the connection to the database.
#' @param init_proj a character string that specifies the EPSG code of the geographic
#' 					projection of the map file (by default "+init=epsg:4326").
#' @param ext_map   a spatial object of class 'extent' from the 'raster' package
#'				    holding the data on the extreme coordinates of the input object  including margins.
#' @param options_list a list of graphical and other parameters used by the rendering engine.
#' @return mapl     a data frame holding the last record from the map database that has been 
#'					entered to the database by the last function run.
#' @keywords internal

save_map <- function(db, init_proj, ext_map, map_data, options_list = NULL)
{
  # Checking if there is a single map file in the rendering output folder (./ttemp)
  if (!length(list.files(paste0(map_data,"/ttemp")))==1){
    stop("Error! The temp folder holds an inappropriate number of files --- please investigate and leave a single appropriate map file!")    
  }
  
  # Function creating a full file name based on the name stored in the database
  pathname <- function(x){
    paste0(map_data,"/Tiles_", x)   
  }
  
  # Extracting the extension of the map file based on the output options 	
  save_name <- strsplit(x = options_list$output, split = '.', fixed = T)
  extens <- as.character(save_name[[1]][2])
  
  # Setting the storage path and filename  
  if (dbGetQuery(db, "SELECT COUNT(DISTINCT mdir) FROM Maplist") <= 1000){
    mdir <- (dbGetQuery(db, "SELECT MAX(mdir) FROM Maplist") + 1)
    mfile <- paste0((dbGetQuery(db, "SELECT MAX(mfile) FROM Maplist") + 1))
  }
  
  if (dbGetQuery(db, "SELECT COUNT(DISTINCT mdir) FROM Maplist") > 1000){
    mdir <- (dbGetQuery(db, "SELECT min(mdir), count(*) AS filecount 
                        FROM Maplist 
                        GROUP BY mdir 
                        ORDER BY filecount ASC
                        LIMIT 5"))[1]
    mfile <- paste0((dbGetQuery(db, "SELECT MAX(mfile) FROM Maplist") + 1))
  }
  
  # Passing new record to the database
  dbSendQuery(db, paste0("INSERT INTO Maplist (
                         bbox1,
                         bbox2,
                         bbox3,
                         bbox4,
                         CRS,
                         url,
                         xmstyle,
                         dpi,
                         scale,
                         size,
                         mdir,
                         mfile,
                         extens) 
                         VALUES (
                         '",ext_map[1],"' ,'",
                         ext_map[3],"' ,'",
                         ext_map[2],"' ,'",
                         ext_map[4],"' , '",
                         init_proj,"' , '",
                         options_list$url,"' , '",
                         options_list$xmstyle,"' , '",
                         options_list$dpi,"' , '",
                         options_list$mscale,"' , '",
                         options_list$size,"' , '",
                         mdir,"' , '",
                         mfile,"' , '",
                         extens,"')"))
  
  # File manipulation - storing the map file in the folder specified earlier, setting the name and clearing the ttemp folder. 		
  file.rename(list.files(paste0(map_data,"/ttemp"), full.names = T ), paste0(map_data,"/ttemp/",mfile,".",extens))
  file.copy(list.files(paste0(map_data,"/ttemp"), full.names = T ), pathname(mdir))
  file.remove(list.files(paste0(map_data,"/ttemp"), full.names = T))
  
  # Returning the last record from the database, covering the saved file -- the record gives sufficient information to load the map. 
  mapl <- dbGetQuery(db, "SELECT * FROM Maplist WHERE ID = (SELECT MAX(ID) FROM Maplist)")
  return(mapl)    
}
#####################################################################
# Load_map function
#####################################################################
#' Load and plot map file given the database record. 
#' 
#' The \code{load_map} function loads the map raster background and plots it
#' using the R graphics device. 
#'
#' @param ext_map a spatial object of class 'extent' from the 'raster' 
#'                package holding the data on the extreme coordinates of 
#'                the input object including a preset margin.
#' @param mapl a data frame holding one record from the map database.
#' @param map_data a character string with the path to the folder storing the map data.
#' @keywords internal

load_map <- function(mapl, map_data, ext_map)
{
  # Loading the file
  gmap <- brick( system.file(paste0(map_data,"/Tiles_",mapl$mdir), paste0(mapl$mfile,".",mapl$extens), package = "gisserver" ))
  
  # Assigning spatial attributes to the map
  ext <- extent(c(mapl$bbox1, mapl$bbox3, mapl$bbox2, mapl$bbox4))
  extent(gmap) <- ext
  proj4string(gmap) <- mapl$CRS
  gmap <- crop(gmap, ext_map)
  
  plotRGB(gmap, interpolate = T, maxpixels = ncell(gmap))   
}

#####################################################################
# Display_map_info
#####################################################################
#' Display the list of available maps.
#' 
#' The \code{display_map_info} function fetches all the entries from the MapID database
#' and prints them.
#' 
#' @param map_data a character string with the path to the folder storing the map data.
#' @export

display_map_info <- function(map_data = "inst/extdata")
{
  # Switching to the server map folder and establishing connection with database
  db <- establish_con(map_data)
  
  # Fetching and printing the entries 
  mapl <- dbGetQuery(db, "SELECT * FROM Maplist")
  print (mapl)
  
  # Disconnect from the database
  dbDisconnect(db)    
}

#####################################################################
# Delete_map function
#####################################################################
#' Erase map from the map server.
#' 
#' The \code{delete_map} function erases a selected map entry from the
#' database and the map file from the map folders.
#' 
#' @param id a numeric indicating the primary key (ID) of the database entry
#'           concerning a specific map. 
#' @param map_data a character string with the path to the folder storing the map data.
#' @export

delete_map <- function(id, map_data = "inst/extdata")
{
  # Switching to the server map folder and establishing connection with database
  db <- establish_con(map_data)
  
  # Fetching the entry with the specified ID, extracting the file path and deleting
  mapl <- dbGetQuery(db, paste0("SELECT * FROM Maplist WHERE ID=", id))
  file_name <- paste0(map_data,"/Tiles_", mapl$mdir,"/", mapl$mfile,".", mapl$extens)   
  file.remove(file_name)
  
  # Erasing the database entry
  dbSendQuery(db, paste0("DELETE FROM Maplist WHERE ID=", id))
  
  # Disconnect from the database
  dbDisconnect(db)    
}

#####################################################################
# Validate_mapid function
#####################################################################
#' Check that stored map info is not corrupt.
#' 
#' The \code{validate_mapid} function checks if the entries in the MapID database
#' match the files stored in the map folders and erases the files without a matching
#' entry; if there are more entries than files, then the missing map files are being
#' rendered.
#' 
#' @param map_data a character string with the path to the folder storing the map data,
#' a default value has been set in the function.   
#' @export                 

validate_mapid <- function(map_data = "inst/extdata")
{
  # Switching to the server map folder and establishing connection with database
  db <- establish_con(map_data)
  
  # Fetching the full file paths based on the database entries
  mapl <- dbGetQuery(db, "SELECT * FROM Maplist")
  mapl_path <- paste0(map_data,"/Tiles_", mapl$mdir,"/", mapl$mfile,".", mapl$extens) 
  mapl <- cbind(mapl, mapl_path)
  
  # Fetching the full file paths based on a query from the server map directories
  filel <- list.files(path = map_data, recursive = TRUE, full.names = TRUE)
  filel <- setdiff(filel, paste0(map_data, "/MapID.db"))
  
  # Function creating a full file name based on the name stored in the database
  pathname <- function(x){
    paste0(map_data, "/Tiles_", x)   
  }
  
  # Searching for files withot a matching entry and entries without a matching file 
  file_name_del <- setdiff(filel, mapl_path)
  mapl_rend <- setdiff(mapl_path, filel)
  
  # Deleting the files without a matching entry
  if (length(file_name_del)>0){
    file.remove(file_name_del)
    print(paste0("Files ", file_name_del," have been removed"))
  }    
  
  # Rendering the maps with a missing map file    
  if (length(mapl_rend)>0){
    mo <- match(mapl_rend, mapl$mapl_path)
    mapl <- mapl[mo,]
    
    for (i in 1:nrow(mapl)){
      mapl <- mapl[i,]
      
      # Extracting the extent of the map and the options used for rendering
      ext_map = list(mapl$bbox1, mapl$bbox3, mapl$bbox2,  mapl$bbox4)
      
      options_list  <-  list(mscale = mapl$scale,
                             url = NULL,
                             wld = F,
                             dpi = mapl$dpi,
                             paper = NULL,
                             margin = NULL,
                             size = NULL,
                             xmstyle = "C:/GIS_data/osm_mapnik/gis.xml",
                             output  = paste0("map.",mapl$extens),
                             tiles = F,
                             just_tiles = F,
                             force_rend = FALSE)
      
      # Rendering the map                
      render_map(map_data = map_data,
                 ext_map = ext_map,
                 db = db,
                 options_list = options_list)
      
      print(paste0("Maps ", mapl$mapl_path," have been rendered"))
      
      # File manipulation - storing the map file in the folder specified earlier, setting the name and clearing the ttemp folder 		
      file.rename(list.files(paste0(map_data,"/ttemp"), full.names = T ), paste0(map_data,"/ttemp/", mapl$mfile,".",mapl$extens))
      file.copy(list.files(paste0(map_data,"/ttemp"), full.names = T ), pathname(mapl$mdir))
      file.remove(list.files(paste0(map_data,"/ttemp"), full.names = T))
    }             
  }
  
  if (length(file_name_del) == 0 & length(mapl_rend) == 0){
    print("No errors were detected in the database")
  }
  
  # Disconnect from the database
  dbDisconnect(db)    
}

#####################################################################
# Establish_con function
#####################################################################
#' Establish connection to the map database (default name 'MapID').
#' 
#' The \code{establish_con} function checks the existence of the map
#' database and establishes a connection which can be then passed on
#' to other functions. The required information is the path to the
#' directory holding map data and optionally a name of the database, 
#' if it is different than default.
#' 
#' @param map_data a character string with the path to the folder 
#' 				   storing the map data, a default value has been set
#' 				   in the function.
#' @param mapdb_name a character string with the name of the map 
#'  				 database.
#' @keywords internal
#' @return db a SQLiteConnection object.
#' @export

establish_con <- function(map_data = "extdata",
                             mapdb_name = "MapID.db")
   { 
     drv <- DBI::dbDriver("SQLite")
     db <- RSQLite::dbConnect(drv, dbname = system.file(paste0(map_data), paste0(mapdb_name), package = "gisserver"))
 
     return(db)
   }

#####################################################################
# Extent_margin function
#####################################################################
#' Creates a bounding box with a margin around the network object.
#' 
#' The \code{extent_margin} transforms the bounding box of the 
#' desired map to include a margin around the network object.
#' 
#' @param ext_map a spatial extent object provided by the get_background
#'                function.
#' @param margin a percentage measure of extent expansion versus the plotted object (0.1 is 10\%).
#' @keywords internal
#' @return ext_map an object of the spatial class 'extent'.


extent_margin <- function(ext_map, margin)
{
  ext_tmp <- ext_map
  
  ext_tmp[1] <- ext_map[1] - margin * (ext_map[2] - ext_map[1])
  ext_tmp[2] <- ext_map[2] + margin * (ext_map[2] - ext_map[1])    
  ext_tmp[3] <- ext_map[3] - margin * (ext_map[4] - ext_map[3])
  ext_tmp[4] <- ext_map[4] + margin * (ext_map[4] - ext_map[3])
  
  ext_map <- ext_tmp
  
  return(ext_map)
}

#####################################################################
# Display_map function
#####################################################################
#' Load and plot map file given the database record ID. 
#' 
#' The \code{display_map} function loads the map raster background and plots it
#' using the R graphics device. 
#'
#' @param id an integer holding the number of the record in the map database.
#' @param map_data a character string with the path to the folder storing the map data.
#' @param mapdb_name a character string with the name of the map 
#'  				 database.
#' @export

display_map <- function(id,
                        map_data = "inst/extdata",
                        mapdb_name = "MapID.db")
{
  # Establishing connection with the database
  db <- establish_con(map_data = map_data,
                      mapdb_name = mapdb_name)
  
  # Fetching the entry with the specified ID, extracting the file path and deleting
  mapl <- dbGetQuery(db, paste0("SELECT * FROM Maplist WHERE ID=", id))
  
  # Loading the file
  gmap <- brick(paste0(map_data,"/Tiles_",mapl$mdir,"/",mapl$mfile,".",mapl$extens))
  
  # Assigning spatial attributes to the map
  ext <- extent(c(mapl$bbox1, mapl$bbox3, mapl$bbox2, mapl$bbox4))
  extent(gmap) <- ext
  proj4string(gmap) <- mapl$CRS
  
  plotRGB(gmap, interpolate = T, maxpixels = ncell(gmap))   
}

