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
