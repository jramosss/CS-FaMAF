import React from 'react';
import { Head } from './Head';
import { Link, Redirect } from 'react-router-dom';
import verifyEmail from '../../services/verification';
import { sendRequest } from '../../services/request';
import { userContext } from '../../user-context';
import Cookies from 'js-cookie';
import jwt_decode from 'jwt-decode';
import { SetCookies } from '../utils/SetCookies';
import '../../custom.css';

const NOT_VALID_EMAIL = -1
const BAD_REQUEST = 400
const UNAUTHORIZED = 401
const ALL_EMPTY = 1
const OTHER_ERROR = 2;

/* Login	/users/	POST		{email,password}	Token	200 OK-401 UNAUTHORIZED-400 BAD REQUEST */ 



/* Login have the form */
class Login extends React.Component {
  constructor(props) {
    super(props);

    this.state = {
      email: '',
      psw: '',
      redirect: false,
      auth: false,
      validations: { 
                    emailValid: '', 
                    pswValid: ''
                  }
    }
   
    this.handleLogin = this.handleLogin.bind(this);
    this.tokenDecode = this.tokenDecode.bind(this);
    this.handleError = this.handleError.bind(this);
  }
  
  static contextType = userContext;


  /* Decode the token and fill the user context */
  tokenDecode() {

    const uData = jwt_decode(this.context.token);
    
    this.context.setUsername(uData.username);
    this.context.setEmail(uData.email);
   
    SetCookies("user",this.context.username,this.context.token,this.context.email,this.context.icon)
    
    this.setState({redirect: true});
  }
  /* Simple error handler function, shows messages in the divs that are below the fields */
  handleError(status, detail){
      let emailValid = document.getElementById('emailValid')
      let pswValid = document.getElementById('pswValid')
      let allEmpty = document.getElementById('allEmptyValid')
      let formatedDetail = "* " + detail
      // Restart messages
      var msgElements = document.getElementsByClassName("validation")
      Array.from(msgElements).forEach(element => {
          element.innerText = ""
      });
      switch (status){
        case OTHER_ERROR:
          allEmpty.innerText = formatedDetail
          break
        case NOT_VALID_EMAIL:
          emailValid.innerText = formatedDetail
          break
        case BAD_REQUEST:
          emailValid.innerText = formatedDetail
          break
        case UNAUTHORIZED:
          pswValid.innerText = formatedDetail
          break
        case ALL_EMPTY:
          allEmpty.innerText = formatedDetail
          break
        default:
          Array.from(msgElements).forEach(element => {
            element.innerText = ""
          });
          break
          
      }
      Array.from(msgElements).forEach(element => {
        element.style.display = "block"
      });
  }
  /* Here i want to stablish the connection with the endpoint for login.
  I think that i need to add redux for this.*/
  handleLogin(e) {
    // Message dissapear 
    var msgElements = document.getElementsByClassName("validation")
    Array.from(msgElements).forEach(element => {
        element.style.display = 'none'
    });

    e.preventDefault();
    
    const email = this.state.email;
    
    if(this.state.psw === '' || this.state.username === '') {
      this.handleError(ALL_EMPTY,"There are empty fields")
      document.getElementById('inemail').value="";
      document.getElementById('inpsw').value="";
    } else {

      if (verifyEmail(email)) {
        
        const psw = this.state.psw
        const partsOfEmail = email.split('@');
        const firstpart = partsOfEmail[0];
        const secondPart = partsOfEmail[1];
        
        const keys = `grant_type=&username=${firstpart}%40${secondPart}&` + 
          `password=${psw}&scope=&client_id=&client_secret=`;
        
        const headers = {
        Accept: "application/json",
        "Content-Type": "application/x-www-form-urlencoded",
        "Access-Control-Allow-Origin": "*"
        }
        
        // This is the function to comunicate with the REST-API.
        sendRequest("POST", headers, keys, "http://127.0.0.1:8000/users").then(async response => {
          
          // token is an object {access_token, type}
          const data = await response.json();
          if (response.ok){
            const token = data.access_token;
            this.context.setToken(token);

            // Now we decode the token to complete the user context
            this.tokenDecode();
          } else {
            /*document.getElementById('inemail').value="";
            document.getElementById('inpsw').value="";*/
            this.handleError(response.status, data.detail)
          }
          
        }).catch(error => {
          this.handleError(OTHER_ERROR, "There was an error")
        })
      }else{
        this.handleError(NOT_VALID_EMAIL, 'The input does not have a valid e-mail format')
      }
    }
  }

  render() {
    let cookie = Cookies;
    if (cookie || this.state.redirect) {
      try {
	      cookie = cookie.getJSON("user");
      }
	    catch (e) {
		    console.log("jaja")
	    }
    
      if (cookie === undefined) return;
      this.context.setUsername(cookie.username);
      this.context.setEmail(cookie.email);
      this.context.setToken(cookie.token);
      this.context.setIcon(cookie.icon);
      return (<Redirect to='/home'/>);
    } else {
      return (
        <userContext.Consumer> 
          {({token, setToken}) => (
            <div class='login-page'>
              <section>
                <Head/>
                <div class='columns'>
                  <div class='column'>
                    <div class='container px-3'> 

                      <div class='container'>
                        <form onSubmit={this.handleLogin}>
                          <div class='field'>
                            <label class='login-label is-large'> E-mail: </label>
                            <div class='control'>
                              <input  class='login-input is-rounded is-large' id='inemail' type='email' value={this.state.email} 
                              onChange={e => this.setState({email: e.target.value})}/>   
                              <div id='emailValid' class='validation'></div>                
                            </div>
                          </div>
                          <div class='field'>
                            <label class='login-label is-large'> Password: </label>
                            <div class='control'>
                              <input class='login-input is-rounded is-large' id='inpsw' type='password' value={this.state.psw}
                               onChange={e => this.setState({psw: e.target.value})}/>
                              <div id='pswValid' class='validation'></div>
                            </div>
                          </div>
                          <div class='field'>
                            <div id='allEmptyValid' class='validation'></div>
                          </div>
                          <div class='field'>
                            <input class='login-button is-medium is-fullwidht is-rounded mb-2 log-btn-margin' type='submit' value='Login'/> 
                          </div>
                      </form>
                    </div>

                  </div>
                  <div class='column'>
                    <div class='container py-6'>
                      <p class='login-label is-large is-size-2'>
                        Don't have an account yet? 
                        <Link id='toReg' class='login-button is-rounded mx-2' 
                          to={`/registerPage`}> Sign up here </Link> 
                      </p>
                    </div>
                  </div>
                </div>
              </div>
              </section>
            </div>
           
          )} 
        </userContext.Consumer>
      )
  }
}
} export {Login}
