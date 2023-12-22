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
    '$2a$10$uxXr1sHrOnK3bZ2La3cQueLt6gWqxPSnEQErtACifH5PsiKZODgQm',
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
    '$2a$10$6jl0rnj3uU8/.p97cUYPqu8idZRlD7ijuidQnb3K6ML/0cPaUAxoq',
    'Another',
    'Lastname',
    'Corp 2',
    'RECRUITER'
);