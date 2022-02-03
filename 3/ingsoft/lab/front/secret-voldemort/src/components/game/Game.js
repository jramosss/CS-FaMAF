import React from 'react';
import '../../stylesheet/custom.css';
import { sendRequest } from '../../services/request';
import { userContext } from '../../user-context';
import { Dashboard } from './Dashboard';
import { Minister } from './Minister';
import { RoleCharacter } from './RoleCharacter';
import { Director } from './Director';
import { PlayersList } from './PlayersList';
import { DiscardPanel, MemoizedDiscardPanel } from './DiscardPanel';
import { Redirect } from 'react-router-dom';
import { Divination } from './Divination';
import Popup from 'reactjs-popup';
import 'reactjs-popup/dist/index.css';
import '../../custom.css';
import '../../popup_custom.css';
import { Avadakedavra } from './Avadakedavra/Avadakedavra';
import { Chat } from "../utils/Chat";
import { Crucio } from './Crucio'
import { Imperio } from './Imperio'
import { ChaosCounter } from "./Chaos";
import { Expelliarmus } from './Expelliarmus';
import { MemoizedVote } from './Vote';
import Cookies from 'js-cookie';

const OTHER_ERROR = -1;
const NOT_IN_ROOM = 403;

class Game extends React.Component{
    constructor(props){
        super(props)
        this.state = {
            n_of_players: 5, // This is the Total number of players
            room_name : '',
            my_role : '',
            voldemort: '',
            death_eaters: [],
            player_list : [],
            de_procs: 0,
            fo_procs: 0,
            my_char: 'asjdjasd',
            minister: '',
            director : '',
            last_minister: '',
            last_director: '',
            votes: [],
            phase: -1,
            timer: null,
            redirect: false,
            redirectPath: '',
            modalText: '',
            isAlive: true,
            chat: [],
            chaos: 0
        }

        this.update = this.update.bind(this)
        this.handleMessages = this.handleMessages.bind(this)
    }
    static contextType = userContext;
    // uncomment when the endpoint is done.
    
    handleMessages(status, detail){
        let btnModal = document.getElementById('btnModal')
        switch (status) { 
          case OTHER_ERROR:
            this.setState({modalText: detail})
            break
          case NOT_IN_ROOM:
            this.setState({
              modalText: detail,
              redirectPath: '/home',
              redirect: true
            })
            break
          default:
            this.setState({
              modalText: detail,
              redirectPath: '',
              redirect: false
              })
            break
        } 
        if(this.state.modalText !== '' &&  null !== btnModal) {
          btnModal.click()
        }
    }

    update(headers, room, path) {

     sendRequest('GET', headers, {}, path)
        .then(async response => {const data = await response.json()
        
          //console.log(data)
          if(!response.ok) {
            this.handleMessages(response.status, data.detail)
          } else {
              if(data.phase !== 5 && data.phase !== 6) {
                this.setState({
                  room_name: room,
                  n_of_players: data.n_of_players, // This is the Total number of players
                  my_role: data.my_role,
                  voldemort: data.voldemort,
                  death_eaters: data.death_eaters,
                  player_list : data.player_list,
                  de_procs: data.de_procs,
                  fo_procs: data.fo_procs,
                  my_char: 'niidealoqui',
                  minister: data.minister,
                  director : data.director,
                  last_minister: data.last_minister,
                  last_director: data.last_director,
                  phase: data.phase,
                  votes: data.votes,
                  chat: data.messages,
                  chaos:data.chaos,
                  isAlive: data.player_list.includes(this.context.username)
                })
              } else {
                  if (data.phase === 5) {
                    this.setState({redirect: true, redirectPath: '/de_won'})
                  } else if(data.phase === 6) {
                    this.setState({redirect: true, redirectPath: '/fo_won'})
                  }
              }
              //console.log("Contexto actual: " + this.state.minister)
          }
        })
    }

    componentDidMount(){
        const headers = {
            Accept: "application/json",
            Authorization: "Bearer " + this.context.token,
            "Content-Type": "application/json"
        }
        try {
            // path for getting the game state.
            const room = this.props.match.params.room
            const path = "http://127.0.0.1:8000/" + room.toString() +"/game_state"
            const timer = setInterval(()=> this.update(headers, room, path), 1000);
            this.setState({timer: timer})
        }catch(e){
            this.handleMessages(OTHER_ERROR, "Error obtaining match data")
        }
    }

    isDead(nickname){
      return this.state.player_list.includes(nickname)
    }

    componentWillUnmount(){
        clearInterval(this.state.timer);
    }


    render(){
      if(this.state.redirect || !this.state.isAlive) {
        if (!this.state.isAlive){
          return(<Redirect to='/killed'/>); 
        } else {
          return (<Redirect to={this.state.redirectPath}/>)
        }
      }else if(this.context.token == '' && Cookies.getJSON("user").room_name != ''){
        return <Redirect to={'/joinRoom/' + Cookies.getJSON("user").room_name} />
      } else {
        return(
            // uncomment once its connected with endpoints
          <userContext.Consumer>
            {({ token }) => (
              token ? 
              <section id='game-form'>
                  <div class="container my-1 py-1">
                    <Popup className='alert-modal' trigger={<button id='btnModal' style={{display:"none"}}></button>} modal position='right center'>
                        <p> 
                            {this.state.modalText}
                        </p>
                    </Popup>
                      <h1 class="game-title is-large"> {this.state.room_name}</h1>
                  </div>
                  <div class='container my-1 py-1'>
                          <Dashboard proclam_de = {this.state.de_procs} 
                          proclam_op={this.state.fo_procs} n_of_players={this.state.n_of_players}/>
                  </div>
                  <div class='container my-1 py-1'>
                          <ChaosCounter token={this.context.token} 
                          room_name={this.state.room_name} chaos={this.state.chaos} />         
                  </div>
                  <div class='container my-1 py-1 panel-bg'> 
                    <div class="columns">
                        <div class="column is-5">
                          <Chat room_name={this.state.room_name} 
                            messages={this.state.chat} 
                            token={token}
                            name={this.context.username}
                            minister={this.state.minister}
                            director={this.state.director}
                            phase={this.state.phase}
                          />
                        </div>
                        <div class="column is-2">
                          <RoleCharacter role={this.state.my_role} charac={ this.state.myChar} />
                        </div>
                        <div class="column is-2 align-cntr">
                          <Minister
                            room_name={this.state.room_name} 
                            mail_context={this.context.email}
                            name={this.state.minister} 
                            phase={this.state.phase} 
                            players={this.state.player_list}
                            last_minister={this.state.last_minister}
                            last_director={this.state.last_director}/>
                            
                            <Director name={this.state.director} />
                            
                            {(this.state.phase === 7 && this.state.minister === this.context.username) ?
                            <Divination room_name={this.state.room_name} minister={this.state.minister} />
                            : <div></div>
                          }
                          {(this.state.phase === 8 && this.state.minister === this.context.username) ?
                            <Avadakedavra 
                              room_name={this.state.room_name}
                              players={this.state.player_list}
                              minister={this.state.minister}
                              token={this.context.token}
                            />
                            : <div></div>
                            
                          }
                          {(this.state.phase === 9 && this.state.minister === this.context.username) ?
                              <Imperio
                              room_name={this.state.room_name} 
                              my_name={this.context.username}
                              players = {this.state.player_list}
                            />
                          : <div></div>
                          }
                          {(this.state.phase === 10 && this.state.minister === this.context.username) ?
                            <Crucio 
                              room_name={this.state.room_name} 
                              players = {this.state.player_list}
                              my_name={this.context.username}
                            />
                            : <div></div>
                          }
                        </div>
                        
                        
                        <div class='column is-3'> 
                          <PlayersList players= {this.state.player_list} />
                        </div>
                    </div>
                  </div>
                  <div class='container align-cntr'>   
                    {((this.state.phase === 3 && this.state.minister === this.context.username) 
                      || (this.state.phase === 4 && this.state.director === this.context.username)
                      || (this.state.phase === 12 && this.state.director === this.context.username)) 
                      ? <MemoizedDiscardPanel minister={this.state.minister} 
                        director={this.state.director} 
                        room_name={this.state.room_name} 
                        phase={this.state.phase}
                        de_procs={this.state.de_procs}
                        /> 
                        :
                      <div></div>
                    }
                  </div>
                  <div class='container align-cntr'>
                    { 
                      (this.state.phase === 2) ? 
                      (<MemoizedVote room_name={this.state.room_name} 
                      usersVotes={this.state.votes}
                      token = {this.context.token}
                      director = {this.state.director} 
                       minister = {this.state.minister}/>)
                    :
                      (<div></div>)
                    }
                  </div> 
                  <div class='container'>   
                    { (this.state.phase === 11 && this.state.minister === this.context.username) ?
                      <Expelliarmus room_name={this.state.room_name}/>
                      :
                      <div></div>
                    }
                      
                  </div>
              </section>
              : <Redirect to='/'/>
            )}
          </userContext.Consumer>
        )
      } 
    }


} export { Game }