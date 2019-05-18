alter table notifications add column recurrency_type varchar(50);

update notifications n
set recurrency_type = 'Week'
where n.recurrency_type is null;