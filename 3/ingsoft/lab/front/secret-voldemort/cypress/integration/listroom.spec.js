const lrpath = "http://localhost:3000/listRoom"
const rootpath = "http://localhost:3000/"



/* To run this test is necessary to remove the DB
  from back-end an run test_game.py */

context("See the list of created room and select one to join", () =>{
  describe("User is not logged in", () => {
    it("try to join room", ()=>{
      cy.visit(lrpath)
      cy.get('h2')
      .contains('To play you need to be logged in')
    })
  })

  describe("User is logged in", () =>{
    it("want to see available rooms", ()=>{
      // login
      cy.visit(rootpath)
      cy.get('input[type=email]')
        .type('player0@example.com')
      cy.get('input[type=password')
        .type('Heladera65')
      cy.get('input[type=submit]').click()
      // display list room
      cy.get('button[id=join]').click()
      cy.get('p[id=title]')
        .contains('Select a room')
    })

    
    it("once a player is in list room, want to go back to home", () => {
      // login
      cy.visit(rootpath)
      cy.get('input[type=email]')
      .type('player1@example.com')
      cy.get('input[type=password')
          .type('Heladera65')
        cy.get('input[type=submit]').click()
        // display list room
        cy.get('button[id=join]').click()
        // go back
        cy.get('button[id=back]').click()
        cy.get('h1[id=welcome').contains('Hello player1')
      })
      

      it("Player1 create JoinTEST44 room and Player2 join", () =>{
        // login
        cy.visit(rootpath)
        cy.get('input[type=email]')
        .type('player1@example.com')
        cy.get('input[type=password')
        .type('Heladera65')
        cy.get('input[type=submit]').click()
        // create room and logout
        cy.get('button[id=create]').click()
        cy.get('input[id=inroomname]')
        .type('JoinTEST44')
        cy.get('input[type=submit]').click()
        cy.get('#exitlobby').click()

        // display list room
        cy.get('button[id=join]').click()

        cy.get('button[id=refresh]').click()

        // looks for room called JoinTEST1
        const roomtag = cy.get('div[id=JoinTEST44]')
          .contains('JoinTEST44')

        //enter to the room
        roomtag.get('button[id=JoinTEST44join]').click()
        cy.get('h2[id=title]').contains('Lobby')
      })

      it("user is not verified", () => {
        cy.visit(rootpath);
        cy.get('a[id=toReg]').click();
        cy.get('input[name=nameUser')
          .type('notverifiedtest')
        cy.get('input[name=passUser]')
          .type('Heladera65')
        cy.get('input[name=passUser2]')
          .type('Heladera65')
        cy.get('input[name=mailUser]')
          .type('jerobernardi@gmail.com')
        cy.get('input[name=regBtn').click();
  
        cy.visit(rootpath)
        cy.get('input[type=email]')
          .type('jerobernardi@gmail.com')
        cy.get('input[type=password')
          .type('Heladera65')
        cy.get('input[type=submit]').click()
        cy.get('button[id=join]').click()
        cy.get('button[id=refresh]').click()
        cy.get('button[id=JoinTEST44join]').click()
        cy.get('h1[id=welcome]').contains('Hello notverifiedtest')
        
      })
    })
})