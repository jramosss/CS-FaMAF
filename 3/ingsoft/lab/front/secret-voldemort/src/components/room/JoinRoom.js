import React, { Component } from 'react';
import { sendRequest } from '../../services/request';
import { userContext } from '../../user-context';
import { Redirect } from 'react-router-dom';
import Cookies from 'js-cookie';
import Popup from 'reactjs-popup';
import 'reactjs-popup/dist/index.css';
import '../../custom.css';
import '../../popup_custom.css';

/* This component allows you to join a room through a url, 
or by selecting the room from a list. */
const OTHER_ERROR = -1;
const ALREADY_IN_ROOM = 409;
class JoinRoom extends Component {
  constructor(props) {
    super(props);
    this.state = {
      redirect: false,
      redirectPath: '',
    };
    this.handleErrors = this.handleErrors.bind(this)
  }

  static contextType = userContext;


 handleErrors = (status, detail) => {
    let btnModal = document.getElementById("btnModal")
    switch(status){
      case ALREADY_IN_ROOM:
        this.setState({redirect: true, redirectPath: "./lobbyRoom/" + this.props.match.params.room})
        break
      default:
        btnModal.click()
        setTimeout(this.setState({redirect: true, redirectPath: "/home"}),1500)
        break
    }
  }

  componentWillMount() {
    const cookie = Cookies.getJSON("user");
    if(cookie !== undefined) {
      
      this.context.setUsername(cookie.username);
      this.context.setEmail(cookie.email);
      this.context.setToken(cookie.token);
      this.context.setIcon(cookie.icon);
  
      const link_room = "/lobbyRoom/" + this.props.match.params.room

      const authorizationToken = "Bearer " + cookie.token
      /* Setting the cookie for redirection. */ 
      Cookies.set("user", { //  We need to fill ALL the fields of the cookie, They get lost.
          username: cookie.username,
          token: cookie.token,
          email: cookie.email,
          icon: cookie.icon,
          room_name: this.props.match.params.room // This is the new field.
      })

      const headers = {
        Accept: "application/json",
        Authorization: authorizationToken,
        "Content-Type": "application/json"
      }
      const endpoint = "http://127.0.0.1:8000/room/join/" + this.props.match.params.room

      sendRequest("GET", headers, {}, endpoint)
        .then(async response => {
          const data = await response.json();

          if(!response.ok) {
            //const error = (data && data.message) || response.status;
            return( 
              this.handleErrors(response.status, data.detail)
            )
          }
          this.setState({redirect: true, redirectPath: link_room})
        })
        .catch(error => {
          this.handleErrors(OTHER_ERROR, "There was an error")
        })
    } else {
      this.handleErrors(OTHER_ERROR, "You need to be logged in to enter the room")
    }
  }

  render() {
    if (this.state.redirect) {
      return (<Redirect to={this.state.redirectPath}/>);
    } else {
      return (
        <div>
        <Popup trigger={<button id='btnModal' style={{display:"none"}}></button>} modal position='right center'>
            <p id='modalText'>
            </p>
        </Popup>
        <div></div>
        </div>
      )
    }
  }
} export default JoinRoom