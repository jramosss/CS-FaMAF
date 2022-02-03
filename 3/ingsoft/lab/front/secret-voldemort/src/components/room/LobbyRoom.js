import React from 'react';
import { sendRequest } from '../../services/request';
import { Redirect } from 'react-router-dom';
import { userContext } from '../../user-context';
import Popup from 'reactjs-popup';
import 'reactjs-popup/dist/index.css';
import '../../custom.css';
import '../../popup_custom.css';
import { Chat } from "../utils/Chat";
import Cookies from 'js-cookie';

class LobbyRoom extends React.Component{
    constructor(props){
        super(props)
        this.state = {
            room_name : this.props.match.params.room,
            players : ["Cargando usuarios conectados"],
            owner : '',
            start: false,
            redirectPath: '/gameRoom/' + this.props.match.params.room,
            timer: null,
            modalText: '',
            chat: [],
            exit: false
        }
        this.getGameState = this.getGameState.bind(this);
        this.handleStart = this.handleStart.bind(this);
        this.handleErrors = this.handleErrors.bind(this);
    }


    static contextType = userContext

    handleErrors(detail){
        this.setState({ modalText: detail })
        let btnModal = document.getElementById("btnModal")
        if(btnModal != null){
            btnModal.click()
        }
        
    }

    getGameState(headers, path){
        try {
            const timerId = setInterval((h=headers,p=path) => {sendRequest('GET', h, {}, p).then(
                async response => {
                    if(!response.ok){ 
                        this.handleErrors(response.detail)
                    }else{
                        const data = await response.json()
                        if(data.room_status === "In game") {
                            this.setState({start: true})
                        }
                        const users = data.users;
                        this.setState({
                            owner: data.owner,
                            players: users,
                            chat: data.messages
                        })

                        console.log(this.context.username + "  +  " + this.state.owner)
                    }
                }
            ).catch(error => {
                console.error("There was an error", error) 
            })
            }, 2000);
            this.setState({timer: timerId});
        }catch(e){
            this.handleErrors("Error obtaining users data. Please ask for the support team")
        }
    }


    componentWillMount(){
        const headers = {
            Accept: "application/json",
            Authorization: "Bearer " + this.context.token,
            "Content-Type": "application/json"
        }
        try{ // without this react explodes 
            const room = this.props.match.params.room // to get props via "redirect" component 
            const path = "http://127.0.0.1:8000/" + room + "/game_state"
            // the component will re-render every setInterval, take care...
            this.getGameState(headers,path)
        }catch(e){
            this.handleErrors("There was an error processing the data, please try again from the platform.")
        }
    }


    componentWillUnmount(){
        clearInterval(this.state.timer);
    }


    handleStart(){
        const headers = {
            Accept: "application/json",
            Authorization: "Bearer " + this.context.token,
            "Content-Type": "application/json"
        }
        const room = this.state.room_name
        const path = "http://127.0.0.1:8000/" + room.toString() + "/start";
        sendRequest("PUT", headers, {}, path)
            .then(async response => {
                const data = await response.json()
                if(!response.ok){ 
                    this.handleErrors(data.detail)
                }else{
                    this.setState({start: true})
                }
            })
            .catch(error => {
                this.handleErrors("There was an error at" + path.toString())
            })
    }


    handleExit(token){

        clearInterval(this.state.timer)
        console.log(token);
        const path = `http://localhost:8000/room/leave/${this.state.room_name}`

        const header = {
            Accept: "application/json",
            Authorization: "Bearer " + token
        }

        sendRequest("GET", header, '', path).then(async response =>{
            const data = await response.json();

            if (response.ok) {
                console.log("Player exit succesfuly");
            } else {
                console.log("Player exit unsuccesfuly: " + data.detail);
            }
        }).catch(error => console.log("There was an error on player's exit: " + error.detail));

        this.setState({exit: true});
    }


    render(){
        
        return(
            <userContext.Consumer>
            {({ token }) => (
            token && !(this.state.exit) ? (this.state.start ? (<Redirect to={{
                pathname: this.state.redirectPath,
                state: { room: this.state.room_name }
            }}
                />) :
            (<section>
                <div class="container room-bg my-3 py-3">
                    <Popup className='alert-modal' trigger={<button id='btnModal' style={{display:"none"}}></button>} modal position='right center'>
                        <p>
                            {this.state.modalText}
                        </p>
                    </Popup>
                    <div class='container has-text-centered'>
                        <h2 id='title' class='room-title'>Lobby</h2>
                    </div>
                    <div class='columns py-5'>
                        <div class='column is-4'>
                            <div class='room-card'>
                                <div class='card-header py-0 my-1'>
                                    <h3 class='card-header-title is-centered'>Players in room</h3>
                                </div>
                                <div class='card-content py-0 my-1 is-centered'>
                                    <ul class='player-list' name='players-list' id='unique-list'>
                                    {this.state.players.map(item => {
                                        return <li class='i-playerlist' id={item}>{item}</li>;
                                    })}
                                    </ul>
                                </div>   
                            </div>
                        </div>
                        <div class='column is-4 has-text-centered'>
                            <h1 class='room-title'> {this.state.room_name} </h1> 
                        </div>
                        <div class='column is-4'>
                            {(this.context.username === this.state.owner) ? 
                                <input class='room-button is-fullwidth my-2' type='button' value='Start Game' onClick={this.handleStart}/> 
                            :   
                                ""
                            }
                            <input id='exitlobby' class='room-button is-fullwidth my-2' type='button' value='Exit Room' onClick={() => this.handleExit(token)}/>
                        </div>
                    </div>
                </div>
                <div class='container'>
                    <div class='column is-6 is-offset-3'>
                        <Chat room_name={this.state.room_name} token={token} messages={this.state.chat}/>  
                    </div>
                </div>
            </section>))
            : (!this.state.exit && Cookies.getJSON("user").room_name != '') ? (<Redirect to={'/joinRoom/' + Cookies.getJSON("user").room_name} />)
            : (this.state.exit) ? <Redirect to='/'/> :
            <Redirect to='/'/>
            )}
            </userContext.Consumer>
        )
    }
    
} export {LobbyRoom}