CREATE TABLE users(
  email text NOT NULL,
  hashedPassword text NOT NULL,
  firstName text,
  lastName text,
  company text,
  role text NOT NULL
);

ALTER TABLE users
ADD CONSTRAINT pk_users PRIMARY KEY (email);

INSERT INTO users(
    email,
    hashedPassword,
    firstName,
    lastName,
    company,
    role
) VALUES (
    'someone@gmail.com',
    'somepassowrd',
    'Someone',
    'His lastname',
    'Corp',
    'ADMIN'
);

INSERT INTO users(
    email,
    hashedPassword,
    firstName,
    lastName,
    company,
    role
) VALUES (
    'another@gmail.com',
    'anotherpassword',
    'Another',
    'Lastname',
    'Corp 2',
    'RECRUITER'
);