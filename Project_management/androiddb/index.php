<?php
    
    require_once 'user.php';
    
    $username = "";
    
    $password = "";
    
    $email = "";
	
	$query = "";
	
	$_POST = json_decode(file_get_contents('php://input'), true);
    
    if(isset($_POST['username'])){
        
        $username = $_POST['username'];
        
    }
    
    if(isset($_POST['password'])){
        
        $password = $_POST['password'];
        
    }
    
    if(isset($_POST['email'])){
        
        $email = $_POST['email'];
        
    }
	
    if(isset($_POST['query'])){
        
        $query = $_POST['query'];
        
    }	
    
    
    
    $userObject = new User();
    
    // Registration
    
    if(!empty($username) && !empty($password) && !empty($email)){
        
        $hashed_password = md5($password);
        
        $json_registration = $userObject->createNewRegisterUser($username, $hashed_password, $email);
        
        echo json_encode($json_registration);
        
    }
    
    // Login
    
    if(!empty($username) && !empty($password) && empty($email) && empty($query)){
        
        $hashed_password = md5($password);
        
        $json_array = $userObject->loginUsers($username, $hashed_password);
        
		// If success start new session
		if ($json_array['success']==1) {
			
		session_start(); // Store data in session variables 
		$_SESSION["loggedin"] = true; 
		$_SESSION["id"] = session_id(); 
		$_SESSION["username"] = $username;
		$_SESSION["hashedpassword"] = $hashed_password;
		
		$json_array['session'] = $_SESSION["id"];
		}
		
		echo json_encode($json_array);
    }
	
    // General query 
    
    if(!empty($username) && !empty($password) && empty($email) && !empty($query)){
        
        $hashed_password = md5($password);
        
        $json_array = $userObject->generalQuery($username, $hashed_password,$query);
        		
		if ($json_array['success']==1) {
		// jesli sie udalo to mozemy cos jeszcze tutaj zrobc
		// przed zwroceniem wynikow	
		}
		
		echo json_encode($json_array);
    }	
	
    ?>