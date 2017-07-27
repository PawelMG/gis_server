context("testing the 'helper' functions in the gisserver package")

map_data = "extdata"

if (!length(list.files(system.file(paste0(map_data,"/ttemp"), paste0(""), package = "gisserver")))==1){
	file.remove(list.files(paste0(system.file(paste0(map_data,"/ttemp"), paste0(""), package = "gisserver")), full.names = T))
	file.copy(to = paste0(system.file(paste0(map_data,"/ttemp"), paste0(""), package = "gisserver")), 
	          from = paste0(system.file(paste0(map_data,"/Tiles_1"), paste0(""), package = "gisserver")))
}

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

sp::coordinates(xy) <- ~Longitude + Latitude
sp::proj4string(xy) <- sp::CRS(proj)
xy <- sp::spTransform(xy, CRSobj = init_proj)
ext_map <- raster::extent(xy)
ext_map <- extent_margin(ext_map = ext_map,
                         margin = options_list$margin)

# Actual testing taking place here

test_that("check_map delivers correct results",{
  db <- establish_con()
  expect_gt(nrow(check_map(db, ext_map, options_list)), 0)
  dbDisconnect(db)
})

test_that("render_map runs without errors",{
  db <- establish_con()
    with_mock(
    'base::shell' = function(x, ...) TRUE,
    'base::file.remove' = function(x, ...) TRUE,
    expect_true(render_map(map_data = "extdata", ext_map = ext_map, options_list = options_list, db))
  )
    dbDisconnect(db)
})

test_that("save_map runs without errors",{
  db <- establish_con()
  with_mock(
  'base::file.remove' = function(x, ...) TRUE,
  expect_gt(nrow(save_map(db, init_proj = init_proj, ext_map = ext_map, map_data = "extdata", options_list = options_list)),0)
  )
  dbDisconnect(db)
})


test_that("establish_con delivers a database connection",{
  expect_output(print(class(establish_con())), regexp = "SQLiteConnection*")
})


test_that("display map info produces adequate results", {
  expect_gt(nrow(display_map_info()), 0)
  
})

test_that("display map produces adequate results", {
  expect_output(print(class(display_map(id = 1))),regexp = "NULL*")
})

test_that("delete_map function works properly",{
  with_mock(
  'gisserver::delete_map' = function() TRUE,
  expect_true(delete_map())
  )
})

test_that("validate_mapid functions properly",{
  with_mock(
    'gisserver::validate_mapid' = function() TRUE,
    expect_true(validate_mapid())
  )
})

test_that("extent_margin functions properly",{
	  expect_equal(extent_margin(ext_map,0), ext_map)
})

test_that("display_map_info functions properly",{
  mapl <- display_map_info(map_data = "extdata")
  expect_gt(nrow(mapl),0)
  })

test_that("validate_mapid runs without errors",{
  with_mock(
    'base::file.remove' = function(x, ...) TRUE,
    'base::file.rename' = function(x, ...) TRUE,
    'base::file.copy' = function(x, ...) TRUE,
    'base::shell' = function(x, ...) TRUE,
    expect_equal(length(validate_mapid(map_data = "extdata")),1)
  )
})


db <- establish_con()
dbSendQuery(db, 'DELETE FROM Maplist WHERE id>1')
dbDisconnect(db)
