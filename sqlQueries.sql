CREATE DATABASE navifactors;
CREATE TABLE factors (
 factor_id INT AUTO_INCREMENT PRIMARY KEY,
 route_id INT,
 time INT,
 day INT,
 cars INT,
 lanes INT,
 zones INT,
 events INT,
 weather INT,
 distance INT,
 FOREIGN KEY (route_id) REFERENCES route(route_id)
);
CREATE TABLE navifactors.routes (
 route_id INT AUTO_INCREMENT PRIMARY KEY,
 location VARCHAR(100),
 destination VARCHAR(100)
);
CREATE TABLE navifactors.streets (
 street_id INT AUTO_INCREMENT PRIMARY KEY,
 street_name VARCHAR(100),
 longitude DOUBLE,
 latitude DOUBLE
 );
CREATE TABLE navifactors.edges (
 edge_id INT AUTO_INCREMENT PRIMARY KEY,
 start_vertex INT,
 end_vertex INT
);
ALTER TABLE factors AUTO_INCREMENT = 1;

ALTER TABLE route AUTO_INCREMENT = 1;
