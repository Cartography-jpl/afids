-- Host: localhost    Database: afidsdb
-- Server version	5.0.45

DROP TABLE IF EXISTS `afids_users`;
CREATE TABLE `afids_users` (
  `id` int(11) NOT NULL auto_increment,
  `username` varchar(50) NOT NULL,
  `password` varchar(50) NOT NULL,
  `dir` varchar(40) NOT NULL,
  `admin` varchar(3) NOT NULL,
  `notes` varchar(2000) NOT NULL,
  PRIMARY KEY  (`id`),
  UNIQUE KEY `username` (`username`)
) ENGINE=MyISAM AUTO_INCREMENT=4 DEFAULT CHARSET=latin1;

LOCK TABLES `afids_users` WRITE;
INSERT INTO `afids_users` VALUES
(1, 'user0','user0','user0','no',''),
(2, 'user1','user1','user1','no',''),
(3, 'user2','user2','user2','no',''),
(4, 'user3','user3','user3','no',''),
(5, 'user4','user4','user4','no',''),
(6, 'user5','user5','user5','no',''),
(7, 'user6','user6','user6','no',''),
(8, 'user7','user7','user7','no',''),
(9, 'user8','user8','user8','no',''),
(10,'user9','user9','user9','no','');
UNLOCK TABLES;
