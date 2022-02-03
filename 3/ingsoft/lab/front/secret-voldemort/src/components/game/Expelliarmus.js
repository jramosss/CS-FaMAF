import React  from 'react';
import { sendRequest } from '../../services/request';
import { userContext } from '../../user-context';
import Popup from 'reactjs-popup';
import '../../popup_custom.css';

class Expelliarmus extends React.Component{

    constructor(props){
        super(props)
        this.state = {
          room_name: ''
        }
        this.accept_expelliarmus = this.accept_expelliarmus.bind(this)
        this.reject_expelliarmus = this.reject_expelliarmus.bind(this)
    }

    static contextType = userContext;

    componentDidMount(){
      this.setState({room_name: this.props.room_name});
      let btnExpelliarmus = document.getElementById("openPopup")
      btnExpelliarmus.click()
    }
  
    /* When the user clicks, the backend is notified so
    that the game can be put into the Expelliarmus phase */
  
    reject_expelliarmus(e){
  
      e.preventDefault()
  
      const authorizationToken = "Bearer " + this.context.token
  
      const headers = {
        Accept: "application/json",
        Authorization: authorizationToken,
        "Content-Type": "application/json"
      }
  
      const keys = {
        "vote": "Nox"
      }
  
      const path = "http://127.0.0.1:8000/" + this.state.room_name + "/expelliarmus"
  
      sendRequest('PUT', headers, keys, path).then(async response => {
        const data = await response.json();
        if(!response.ok){ 
          return console.log("some error")
        }else{
          return console.log("Reject cast expelliarmus.")
        }
      }).catch(error => {
          return console.log("Ups! something went wrong.")
      })
  
    }
    
    accept_expelliarmus(e){
      e.preventDefault()
  
      const authorizationToken = "Bearer " + this.context.token
  
      const headers = {
        Accept: "application/json",
        Authorization: authorizationToken,
        "Content-Type": "application/json"
      }
  
      const keys = {
        "vote": "Lumos"
      }
  
      const path = "http://127.0.0.1:8000/" + this.state.room_name + "/expelliarmus"
  
      sendRequest('PUT', headers, keys, path).then(async response => {
        const data = await response.json();
        if(!response.ok){ 
          return console.log("some error")
        }else{
          return console.log("Confirm cast expelliarmus.")
        }
      }).catch(error => {
          return console.log("Ups! something went wrong.")
      })
    }
  
    render(){
      return (
        <div class="align-cntr">
            <Popup className='divination-modal' 
                  trigger={<button id='openPopup' class="btn-spell"></button>}
                  modal position='right center' 
                  closeOnDocumentClick={false} 
                  closeOnEscape={false}>
                {(close) => (
                  <div class='container has-text-centered'>
                  <h3 class='room-title'>The director said Expelliarmus, and you?</h3>
                  <button class='panel-button is-medium mx-3' name='acceptoExp' onClick={this.accept_expelliarmus} onClickCapture={close}>¡Expelliarmus!</button>
                  <button class='panel-button is-medium mx-3' name='rejectoExp' onClick={this.reject_expelliarmus} onClickCapture={close}>Reject</button>
                  </div>
                )}
            </Popup>
        </div>
      )
    }
  } export { Expelliarmus }