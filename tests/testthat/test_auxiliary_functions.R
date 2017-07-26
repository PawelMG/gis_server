context("testing the 'helper' functions in the gisserver package")

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

test_that("load_map functions properly",{
  mapl <- display_map_info(map_data = "extdata")
  a <- class(load_map(mapl, map_data = "extdata", ext_map))
  expect_equal(a, "NULL")
})