table! {
    game (id) {
        id -> Int8,
        uid -> Varchar,
        player_uid -> Varchar,
        json -> Varchar,
    }
}

table! {
    lists (name) {
        name -> Varchar,
        json -> Text,
    }
}

table! {
    user (id) {
        id -> Int8,
        user_id -> Varchar,
        name -> Varchar,
        email -> Varchar,
        json -> Varchar,
    }
}

allow_tables_to_appear_in_same_query!(
    game,
    lists,
    user,
);
