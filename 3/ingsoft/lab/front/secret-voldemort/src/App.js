import React, { Component } from 'react';
import './stylesheet/App.sass';
import { Login } from './components/user/Login';
import { BrowserRouter as Router , Route } from 'react-router-dom';
import { userContext } from './user-context';
import { Home } from './components/user/Home';
import { Register } from './components/user/Register';
import { LobbyRoom } from './components/room/LobbyRoom';
import { Game } from './components/game/Game';
import CreateRoom from './components/room/CreateRoom';
import JoinRoom from './components/room/JoinRoom';
import { ChangeNickname, ChangePassword } from './components/user/Profile';
import { De_won } from './components/game/De_won';
import { Fo_won } from './components/game/Fo_won';
import { ListRoom } from './components/room/ListRoom';
import { Killed } from './components/game/Killed';
import { ChaosCounter } from './components/game/Chaos';
// This is the "main".
// We use rout for organization of our single page app.
class App extends Component {
  constructor(props) {
    super(props);
  }

  static contextType = userContext

  setToken = token => {
     this.setState({token});
  }
  
  setUsername = username => {
    this.setState({username});
  }

  setEmail =  email => {
    this.setState({email});
  }

  setIcon = icon => {
    this.setState({icon});
  }

  state = {
    token: '',
    username: '',
    email: '',
    icon: {},
    setToken: this.setToken,
    setUsername: this.setUsername,
    setEmail: this.setEmail,
    setIcon: this.setIcon
  };

  render() {  //<Route exact path='/fo_won' component={Fo_won}/>
    return (
      <Router>
        <userContext.Provider value={this.state}>
          <section>
            <Route exact path='/' component={Login}/>
            <Route exact path='/home' component={Home}/>
            <Route exact path='/registerPage' component={Register}/>
            <Route exact path='/change_nickname' component={ChangeNickname}/>
            <Route exact path='/change_password' component={ChangePassword}/>
            <Route exact path='/createRoom' component={CreateRoom}/>
            <Route exact path='/lobbyRoom/:room' render= {(props) => <LobbyRoom {...props}/>}/>
            <Route exact path='/gameRoom/:room' render= {(props) => <Game {...props}/>}/>
            <Route exact path='/joinRoom/:room' component={JoinRoom}/>
            <Route exact path='/killed' component={Killed}/>
            <Route exact path='/listRoom' component={ListRoom}/>
            <Route exact path='/de_won' component={De_won}/>
            <Route exact path='/fo_won' component={Fo_won}/>
          </section>
        </userContext.Provider>
      </Router>
    );
  }
}
export default App;