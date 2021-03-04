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

alter table notifications alter column dt_to_notificate set not null;

alter table notifications add column kind varchar(50);

update notifications
set kind = 'OneDate'
where kind is null;

alter table notifications alter column kind set not null;

alter table notifications add column period bigint;
alter table notifications add column hour int;
alter table notifications add column minute int;
alter table notifications add column start timestamp;
alter table notifications add column finish timestamp;
-- Normalization needed, but I'm short on time
alter table notifications add column days varchar(20);

alter table notifications add constraint notifications_kind_correct
    check (notifications.kind in ('OneDate', 'Confirmation', 'Periodic'));

alter table notifications add constraint notifications_kind_has_fields
    check (notifications.kind = 'Confirmation' and notifications.period is not null or
           notifications.kind = 'Periodic' and notifications.hour is not null and notifications.minute is not null and
           notifications.days is not null or notifications.kind = 'OneDate');
