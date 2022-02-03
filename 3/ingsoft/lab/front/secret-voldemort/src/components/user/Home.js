
import React, { useContext, useState, useEffect } from 'react';
import { Button } from '../utils/Button';
import Cookies from 'js-cookie';
import { Redirect } from 'react-router-dom';
import { userContext } from '../../user-context';
import App from '../../App';


export function Home(props) {

	const [logged, update] = useState(true);
	const context = useContext(userContext);

	const logOut = () => {
		Cookies.remove("user");
		update(false)
	}

	useEffect(() => {
		if(context.username === '') {
			const cookie = Cookies.getJSON("user");
			if(cookie !== undefined) {
				context.setUsername(cookie.username);
				context.setEmail(cookie.email);
				context.setToken(cookie.token);
				context.setIcon(cookie.icon);
			}
		}
	}, [context])
	console.log(context.token)

	return (
		<div>
			{ (Cookies.get("user") !== undefined) ? (
				logged ? 
				<div>
					<div class='section'>
						<div class='container mt-4'>
							<div class='columns is-desktop is-vcentered'>
								<div class='column'>
									<h1 id='welcome' class='home-title'> Hello {context.username}</h1>
								</div>
								<div class='column'> 
									<Button id='create' style='home-button is-large is-fullwidth m-6' path='/createRoom' text='Create room' type='btncr'/>
									<Button id='join' style='home-button is-large is-fullwidth m-6' path='/listRoom' text='Join room' type='btncr'/>
									<Button id='change_nickname' style='home-button is-large is-fullwidth m-6' path='/change_nickname' text='Change nickname' type='btncn'/>
									<Button id='change_password' style='home-button is-large is-fullwidth m-6' path='/change_password' text='Change password' type='btncp'/>
									<button id='logout' class='home-button is-large is-fullwidth m-6' onClick={logOut}>Logout</button>
								</div>
							</div> 
						</div>
					</div>
					<div class='hero is-bold is-small is-fullwidth'>
						<div class='hero-body has-text-left is-vcentered'>
							<div class='container'>
								<h1 class='hero-title is-1'>Secret Voldemort</h1>
							</div>
						</div>
						
					</div>
				</div>
				:
				<Redirect to='/'/>
				)
				:
				<Redirect to='/'/>
			}
		</div>
		
	);
} 
