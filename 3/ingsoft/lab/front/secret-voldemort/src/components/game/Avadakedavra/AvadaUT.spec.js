import React from 'react';
import { mount } from 'cypress-react-unit-test';
import { Avadakedavra } from './Avadakedavra';

context("Unit test for Avadakedabra component", () => {
  const player = [1,2,3,4,5];
  const minister = 3;

  describe("the game is not in the correct phase", () => {
    it("the minister want to cast Avadakedavra", () => {
      
      const email = 3;
      mount(
      (1 === 8 && email === minister) ?
        <Avadakedavra minister={minister} email={email}/>
        : <div></div>
      )
      cy.get('button[id=trigger]').should('not.exist');
    });
  })

  describe("the game is in the correct phase", () => {
    
    it("A player want to cast avadakedavra but isn't the minsiter", () =>{
      
      const email = 2;
      mount(
        (8 === 8 && email === minister) ?
          <Avadakedavra minister={minister} email={email}/>
          : <div></div>
      )
      cy.get('button[id=trigger]').should('not.exist');
    })

    it("The minister want to cast avadakedvra", () => {
      
      const email = 3;
      mount(
        (8 === 8 && email === minister) ?
          <Avadakedavra phase={8} minister={minister} email={email} players={player}/>
          : <div></div>
      )
      cy.get('button[id=trigger]').click()
      cy.get('p[id=title]')
        .contains("Choose who to cast the spell on")
      player.map(p =>{
        if(p !== 3) {
          cy.get(`li[id=i-${p}]`).contains(p.toString())
          cy.get(`button[id=cast-${p}`).should('exist');
        } else {
          cy.get(`li[id=i-${p}]`).should('not.exist')
        }
      })
    })
  })
})