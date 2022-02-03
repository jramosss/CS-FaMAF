# test_main.py
from fastapi import FastAPI, Depends
from starlette.middleware.cors import CORSMiddleware

from api.routers import users, room_endpoints, user_info, hub_endpoints, spells_endpoints, game_endpoints
from api.models.base import define_database_and_entities
from api.handlers.authentication import verify_token

define_database_and_entities(
    provider='sqlite', filename='test.sqlite', create_db=True)


test_app = FastAPI()


test_app.include_router(users.router)
test_app.include_router(room_endpoints.router, dependencies=[
                        Depends(verify_token)])
test_app.include_router(spells_endpoints.router,
                        dependencies=[Depends(verify_token)])
test_app.include_router(user_info.router, dependencies=[Depends(verify_token)])
test_app.include_router(hub_endpoints.router, dependencies=[
                        Depends(verify_token)])
test_app.include_router(game_endpoints.router, dependencies=[
                        Depends(verify_token)])


test_app.add_middleware(
    CORSMiddleware,
    allow_origins=["*"],
    allow_credentials=True,
    allow_methods=["*"],
    allow_headers=["*"],
)
