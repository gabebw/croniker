-- Define the function to set the "updated_at" column
CREATE OR REPLACE FUNCTION set_updated_at_column()
RETURNS TRIGGER AS $$
BEGIN
    NEW.updated_at = now();
    RETURN NEW;
END;
$$ language 'plpgsql';

-- Create triggers
CREATE TRIGGER set_users_updated_at BEFORE UPDATE ON "users" FOR EACH ROW EXECUTE PROCEDURE set_updated_at_column();
CREATE TRIGGER set_profiles_updated_at BEFORE UPDATE ON "profiles" FOR EACH ROW EXECUTE PROCEDURE set_updated_at_column();
