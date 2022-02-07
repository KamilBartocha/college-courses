<?php
    
    include_once 'config.php';
    
    class DbConnect{
        
        private $connect;
        
        public function __construct(){
            
			try {
            $this->connect = mysqli_connect(DB_HOST, DB_USER, DB_PASSWORD, DB_NAME);
      
            }		
			catch (mysqli_sql_exception $e)
			{
			echo "Unable to connect to MySQL Database: " . mysqli_connect_error();	
			}
        }
        
        public function getDb(){
            return $this->connect;
        }
    }
    ?>