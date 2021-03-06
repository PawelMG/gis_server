context("testing the main function of the package - get background")

map_data = "extdata"

if (!length(list.files(system.file(paste0(map_data,"/ttemp"), paste0(""), package = "gisserver")))==1){
  file.remove(list.files(paste0(system.file(paste0(map_data,"/ttemp"), paste0(""), package = "gisserver")), full.names = T))
  file.copy(to = paste0(system.file(paste0(map_data,"/ttemp"), paste0(""), package = "gisserver")), 
            from = paste0(system.file(paste0(map_data,"/Tiles_1"), paste0(""), package = "gisserver")))
}

test_that("get_background fetches adequate results - case 1",{
  
  # Preparing dummy objects for functions to ineract with
  
  Longitude <- runif(100, min =  20.9,  max = 21.05)
  Latitude <- runif(100, min = 52.2, max = 52.25)
  xy <- as.data.frame(cbind(Longitude, Latitude))
  
  # Setting the projection of the graph coordinates (proj) and the projection of the underlying map (init_proj)
  
  proj = "+init=epsg:4326"
  init_proj = "+init=epsg:4326" # default OSM coordinate reference system
  
  options_list  <- list(mscale = 29000,
                        url = NULL,
                        wld = FALSE,
                        dpi = 200,
                        paper = NULL,
                        margin = 0.01,
                        size = NULL,
                        xmstyle = "C:/GIS_data/osm_mapnik/gis.xml",
                        output  = "map.png",
                        tiles = FALSE,
                        just_tiles = FALSE,
                        force_rend = TRUE)
  with_mock(
    'base::file.remove' = function(x, ...) TRUE,
    a <- get_background(xy = xy,
                        map_data = "extdata",
                        mapdb_name = "MapID.db",
                        proj = proj,
                        init_proj = init_proj,
                        options_list = options_list)
  )
    expect_equal(a,NULL)
})


test_that("get_background fetches adequate results - case 2",{
  
  # Preparing dummy objects for functions to ineract with
  
  Longitude <- runif(100, min =  20.5,  max = 21.05)
  Latitude <- runif(100, min = 52.2, max = 52.25)
  xy <- as.data.frame(cbind(Longitude, Latitude))
  
  # Setting the projection of the graph coordinates (proj) and the projection of the underlying map (init_proj)
  
  proj = "+init=epsg:4326"
  init_proj = "+init=epsg:4326" # default OSM coordinate reference system
  
  options_list  <- list(mscale = 29000,
                        url = NULL,
                        wld = FALSE,
                        dpi = 200,
                        paper = NULL,
                        margin = 0.01,
                        size = NULL,
                        xmstyle = "C:/GIS_data/osm_mapnik/gis.xml",
                        output  = "map.png",
                        tiles = FALSE,
                        just_tiles = FALSE,
                        force_rend = FALSE)
  
  with_mock(
    'base::file.remove' = function(x, ...) TRUE,
    a <- get_background(xy = xy,
                        map_data = "extdata",
                        mapdb_name = "MapID.db",
                        proj = proj,
                        init_proj = init_proj,
                        options_list = options_list)
  )
  expect_equal(a,NULL)
})

test_that("get_background fetches adequate results - case 3",{

  # Preparing dummy objects for functions to ineract with

  Longitude <- runif(100, min =  20.5,  max = 21.05)
  Latitude <- runif(100, min = 52.2, max = 52.25)
  xy <- as.data.frame(cbind(Longitude, Latitude))

  # Setting the projection of the graph coordinates (proj) and the projection of the underlying map (init_proj)

  proj = "+init=epsg:4326"
  init_proj = "+init=epsg:4326" # default OSM coordinate reference system

  options_list  <- list(mscale = 29000,
                        url = NULL,
                        wld = FALSE,
                        dpi = 200,
                        paper = NULL,
                        margin = 0.01,
                        size = NULL,
                        xmstyle = "C:/GIS_data/osm_mapnik/gis.xml",
                        output  = "map.png",
                        tiles = FALSE,
                        just_tiles = FALSE,
                        force_rend = TRUE)
    
  with_mock(
    'base::system' = function(x, ...) TRUE,
    'base::file.remove' = function(x, ...) TRUE,

    a <- get_background(xy = xy,
                        map_data = "extdata",
                        mapdb_name = "MapID.db",
                        proj = proj,
                        init_proj = init_proj,
                        options_list = options_list)

  )
  expect_equal(a,NULL)
})

db <- establish_con()
dbSendQuery(db, 'DELETE FROM Maplist WHERE id>1')
dbDisconnect(db)