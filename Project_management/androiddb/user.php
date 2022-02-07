<?php
    
    include_once 'db-connect.php';
    
    class User{
        
        private $db;
        
        private $db_table = "users";
        
        public function __construct(){
            $this->db = new DbConnect();
        }
        
        public function isLoginExist($username, $password){
            
            $query = "select * from ".$this->db_table." where username = '$username' AND password = '$password' Limit 1";
            
            $result = mysqli_query($this->db->getDb(), $query);
            
            if(mysqli_num_rows($result) > 0){
                
                mysqli_close($this->db->getDb());
                
                
                return true;
                
            }
            
            mysqli_close($this->db->getDb());
            
            return false;
            
        }
        
        public function isEmailUsernameExist($username, $email){
            
            $query = "select * from ".$this->db_table." where username = '$username' AND email = '$email'";
            
            $result = mysqli_query($this->db->getDb(), $query);
            
            if(mysqli_num_rows($result) > 0){
                
                mysqli_close($this->db->getDb());
                
                return true;
                
            }
            
            
            return false;
            
        }
        
        public function isValidEmail($email){
            return filter_var($email, FILTER_VALIDATE_EMAIL) !== false;
        }
        
        
        
        public function createNewRegisterUser($username, $password, $email){
            
            
            $isExisting = $this->isEmailUsernameExist($username, $email);
            
            
            if($isExisting){
                
                $json['success'] = 0;
                $json['message'] = "Error in registering. Probably the username/email already exists";
            }
            
            else{
                
            $isValid = $this->isValidEmail($email);
                
                if($isValid)
                {
                $query = "insert into ".$this->db_table." (username, password, email, created_at, updated_at) values ('$username', '$password', '$email', NOW(), NOW())";
                
                $inserted = mysqli_query($this->db->getDb(), $query);
                
                if($inserted == 1){
                    
                    $json['success'] = 1;
                    $json['message'] = "Successfully registered the user";
                    
                }else{
                    
                    $json['success'] = 0;
                    $json['message'] = "Error in registering. Probably the username/email already exists";
                    
                }
                
                mysqli_close($this->db->getDb());
                }
                else{
                    $json['success'] = 0;
                    $json['message'] = "Error in registering. Email Address is not valid";

                
                }
                
            }
            
            return $json;
            
        }
        
        public function loginUsers($username, $password){
            
            $json = array();
            
            $canUserLogin = $this->isLoginExist($username, $password);
            
            if($canUserLogin){
                
                $json['success'] = 1;
                $json['message'] = "Successfully logged in";
                
            }else{
                $json['success'] = 0;
                $json['message'] = "Incorrect details";
            }
            return $json;
        }

        // funkcja ogolnego przeznacznia - mozna wyslac dowlone zapytanie
        // do bazy danych pod warunkiem, ze user istnieje i haslo jest poprawne 		
		public function generalQuery($username, $password, $query){
                 	  
            $json = array();
            
			// sprawdz czy uzytkownik istnieje i moze sie zalogowac
			$q = "select * from ".$this->db_table." where username = '$username' AND password = '$password' Limit 1";
            $result = mysqli_query($this->db->getDb(), $q);            
            if(mysqli_num_rows($result) > 0){
			// jesli tak to wykonaj query na bazie	
            $json['success'] = 1;
		    $result = mysqli_query($this->db->getDb(), $query); 
            $result = mysqli_fetch_all($result, MYSQLI_NUM);			
            mysqli_close($this->db->getDb());
            $json['message'] = $result;    	
            }
			else { // jesli nie to zwroc blad
			 mysqli_close($this->db->getDb());
             $json['success'] = 0;
             $json['message'] = "Incorrect details";
            }
            return $json;				  
        }
		
    }
    ?>