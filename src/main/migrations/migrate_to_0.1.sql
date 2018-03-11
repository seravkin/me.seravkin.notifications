CREATE TABLE users (
  id SERIAL PRIMARY KEY,
  chat_id INT,
  telegram_name VARCHAR(2000) NOT NULL
);

CREATE UNIQUE INDEX XI_USER_TG_NAME ON users(telegram_name);

CREATE TABLE notifications (
  id SERIAL PRIMARY KEY,
  text VARCHAR(2000) NOT NULL,
  is_active BOOLEAN NOT NULL,
  dt_to_notificate TIMESTAMP,
  id_user INT NOT NULL REFERENCES users
);

CREATE INDEX XI_USER ON notifications(id_user);
CREATE INDEX XI_DT_TO_NOTIFICATE ON notifications(is_active, dt_to_notificate);