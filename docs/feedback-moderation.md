# IEABank feedback moderation: setup

The public Shiny Explorer submits bug reports, incorrect-data reports, and suggestions to the RLS-protected `public.feedback_reports` table in Supabase. An optional PNG/JPEG screenshot (maximum 5 MiB) is stored privately as `bytea`; no screenshot or reporter email is sent to GitHub.

## Admin setup

1. Deploy the latest **main** of `IEABank_Admin_GUI` to Posit Connect Cloud.
2. Create a **fine-grained GitHub personal access token** with access only to `macsyt1/IEABank_User_GUI`, granting **Issues: Read and write** and **Metadata: Read**. Account permissions for organization repositories may require approval.
3. Add the token in the **Admin app's server-side environment variables** under the exact name `GITHUB_ISSUES_TOKEN`. Do not store the token in GitHub source code, the public Shiny app, or its environment.
4. Redeploy/restart the Admin app after adding the variable.
5. Sign in with an account authorized to edit the Item Bank. Select **Feedback review**.
6. Review report contents and screenshot, then choose **Approve and publish GitHub issue** or **Reject report**.

Only an approved report can become a GitHub issue. Reviewers should remove sensitive information from the report description before publication (editing facilities are not yet implemented; reject inappropriate reports and ask for a sanitized resubmission). Reporter email and screenshots stay private in Supabase.

## Security notes

The public PostgreSQL role has INSERT privileges on only the submission fields, and RLS prevents reading reports. `editor_app` can read and update pending reports. The Shiny form validates file signatures, size and required text. A one-minute per-session throttle helps prevent accidental duplicate submissions but is **not** a robust rate limiter; implement an API gateway / CAPTCHA or server-side rate-limiter before opening the form broadly to unauthenticated users.

## Verification

Use the public app to submit a test report; confirm that it appears in **Feedback review**. Verify screenshot display and reject that test report. Then approve a suitable test report and confirm that the GitHub issue URL is stored. Do not publish sensitive screenshots or contact emails.
