# Phoenix CRM MVP

Build an Elixir Phoenix LiveView CRM for managing accounts and contacts.

## Data Model

```
Account (company)
├── name, industry, website, notes
├── has_many :account_contacts (join table)
└── has_many :contacts, through: :account_contacts

Contact (person)
├── first_name, last_name, title, notes
├── has_many :emails (ContactEmail: address, label, primary?)
├── has_many :account_contacts
└── has_many :accounts, through: :account_contacts

AccountContact (join with role)
├── account_id, contact_id
├── role (e.g., "Decision Maker", "Technical Lead")
└── started_at, ended_at (optional, for history)
```

## UI/Colors

Use Tailwind with these custom colors:

```css
/* tailwind.config.js extend colors */
'raspberry': '#950952',    /* Primary actions, buttons */
'crimson': '#5e0035',      /* Headers, accents */
'black': '#020202',        /* Text */
'forest': '#023618',       /* Success states */
'teal': '#005c69',         /* Links, secondary actions */
```

## Phases

### Phase 1: Foundation
- [ ] `mix phx.new crm --live --no-mailer --no-dashboard`
- [ ] Configure Tailwind with custom colors
- [ ] Create migrations: accounts, contacts, contact_emails, account_contacts
- [ ] Create Ecto schemas with associations
- [ ] Seed with sample data

### Phase 2: Accounts CRUD
- [ ] LiveView: list accounts (search, sort)
- [ ] LiveView: show account with linked contacts
- [ ] LiveView: create/edit account form
- [ ] Delete with confirmation

### Phase 3: Contacts CRUD
- [ ] LiveView: list contacts (search, filter by account)
- [ ] LiveView: show contact with all emails and linked accounts
- [ ] LiveView: create/edit contact with nested email forms
- [ ] Add/remove emails dynamically (LiveView form)
- [ ] Link/unlink contacts to accounts with role

### Phase 4: Polish
- [ ] Navigation header with raspberry/crimson theme
- [ ] Empty states, loading states
- [ ] Flash messages styled
- [ ] Basic responsive layout

## Agent Workflow

Use 2-3 agents via beads:

```bash
# Initialize beads
bd init

# Create issues for parallel work
bd create -t "Phase 1: Foundation setup" -p high
bd create -t "Phase 2: Accounts CRUD" -p high
bd create -t "Phase 3: Contacts CRUD" -p medium
bd create -t "Phase 4: UI Polish" -p low

# Set dependencies
bd dep add <phase2-id> <phase1-id>
bd dep add <phase3-id> <phase1-id>
bd dep add <phase4-id> <phase2-id> <phase3-id>

# Launch agents on ready tasks
# Agent 1: Foundation
# Agent 2: Accounts (after foundation)
# Agent 3: Contacts (after foundation, parallel with accounts)
```

## Progress Tracking (P0 Task)

Create a P0 "tracker" issue that agents update as work progresses:

```bash
bd create -t "PROJECT TRACKER - MVP Status" -p critical
# Note the ID (e.g., CRM-1)
```

**Agents must update this task after completing each phase:**

```bash
bd comment CRM-1 "Phase 1 complete. Phase 2+3 in progress. ~60% done. ETA: 2 phases remain."
bd comment CRM-1 "Phase 2 complete. Phase 3 at 80%. ~85% done."
bd comment CRM-1 "All phases complete. MVP DONE."
bd close CRM-1
```

**Host polls from outside:**
```bash
sprite exec -s <sprite> bd show CRM-1
sprite exec -s <sprite> bd comments CRM-1
```

Or a watch script:
```bash
#!/usr/bin/env bash
# sprite-watch-progress <sprite-name>
while true; do
    clear
    echo "=== $(date) ==="
    sprite exec -s "$1" bd show CRM-1 2>/dev/null
    sprite exec -s "$1" bd comments CRM-1 2>/dev/null | tail -5

    # Check if closed
    if sprite exec -s "$1" bd show CRM-1 2>/dev/null | grep -q "status: closed"; then
        echo "PROJECT COMPLETE"
        break
    fi
    sleep 60
done
```

## Success Criteria

- [ ] Can create accounts and contacts
- [ ] Contacts can have multiple emails
- [ ] Contacts can be linked to multiple accounts with roles
- [ ] UI uses the specified color palette
- [ ] All CRUD operations work via LiveView (no page reloads)
