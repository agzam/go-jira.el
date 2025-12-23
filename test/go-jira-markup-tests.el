;;; go-jira-markup-tests.el --- Tests for go-jira-markup -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2024 Ag Ibragimov
;;
;; Author: Ag Ibragimov <agzam.ibragimov@gmail.com>
;; Maintainer: Ag Ibragimov <agzam.ibragimov@gmail.com>
;; Created: December 06, 2024
;; Keywords: tools jira
;; Homepage: https://github.com/agzam/go-jira.el
;;
;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;;
;;  Tests for JIRA markup to Org-mode conversion
;;
;;; Code:

(require 'buttercup)
(require 'go-jira-markup)

(describe "go-jira-markup-to-org"
  
  (describe "nested lists - numbered with bullets"
    
    (it "converts SAC-29730 description correctly"
      (let ((input "Create POST endpoint at /v1/internal/metrics


# Route Location: /v1/internal/metrics (simpler than connection-scoped since metrics apply to jobs across connections)
# Authentication: Use existing auth/validate-token from refresh token (consistent with other internal-orchestrator routes)
# Payload Structure: JSON body containing:
#* Job metadata (tenant_id, connection_id, extraction_spec_id, job_name, tap_name, tap_version)
#* Array of metric points (matching Singer's Point structure: metric_type, metric, value, tags)
# Prometheus Metrics to Create:
#* Counter: menagerie_tap_records_total (labels: tap_name, tap_version, tap_stream_id, connection_id)
#* Histogram: menagerie_tap_http_request_duration_seconds (labels: tap_name, tap_version, endpoint, http_status_code)
#* Histogram: menagerie_tap_job_duration_seconds (labels: tap_name, tap_version, job_type, status)
# Implementation:
#* Add metrics definitions to com/qlik/utils/metrics.clj
#* Add route handler to com/qlik/routes/v1.clj in internal-orchestrator-routes
#* Route handler validates the payload and updates the in-memory Prometheus metrics
"))
        (expect (go-jira-markup-to-org input)
                :to-equal "Create POST endpoint at /v1/internal/metrics

1. Route Location: /v1/internal/metrics (simpler than connection-scoped since metrics apply to jobs across connections)
2. Authentication: Use existing auth/validate-token from refresh token (consistent with other internal-orchestrator routes)
3. Payload Structure: JSON body containing:
  - Job metadata (tenant_id, connection_id, extraction_spec_id, job_name, tap_name, tap_version)
  - Array of metric points (matching Singer's Point structure: metric_type, metric, value, tags)
4. Prometheus Metrics to Create:
  - Counter: menagerie_tap_records_total (labels: tap_name, tap_version, tap_stream_id, connection_id)
  - Histogram: menagerie_tap_http_request_duration_seconds (labels: tap_name, tap_version, endpoint, http_status_code)
  - Histogram: menagerie_tap_job_duration_seconds (labels: tap_name, tap_version, job_type, status)
5. Implementation:
  - Add metrics definitions to com/qlik/utils/metrics.clj
  - Add route handler to com/qlik/routes/v1.clj in internal-orchestrator-routes
  - Route handler validates the payload and updates the in-memory Prometheus metrics")))
    
    (it "converts SAC-29730 comment with deeper nesting correctly"
      (let ((input "Plan outline:

# Add job-level Prometheus metrics (metrics.clj):
#* menagerie_extraction_job_total (Counter) - tracks job completions with labels: tap_name, tap_version, status (succeeded/failed), mode (sync/check)
#* menagerie_extraction_job_duration_seconds (Histogram) - tracks job execution time with same labels
# Create metrics request/response schemas (internal_metrics.clj):
#* Define schema for POST body containing:
#** tap_name, tap_version, mode (sync/check)
#** status (succeeded/failed)
#** duration_seconds (float)
#* Simple 200 OK response on success
# Add metrics ingestion route (v1.clj):
#* Route: POST /v1/internal/metrics/extraction-job
#* Authentication: auth/validate-token (refresh token from orchestrator)
#* Handler increments counter and observes histogram duration
#* Returns {:status 200}
"))
        (expect (go-jira-markup-to-org input)
                :to-equal "Plan outline:

1. Add job-level Prometheus metrics (metrics.clj):
  - menagerie_extraction_job_total (Counter) - tracks job completions with labels: tap_name, tap_version, status (succeeded/failed), mode (sync/check)
  - menagerie_extraction_job_duration_seconds (Histogram) - tracks job execution time with same labels
2. Create metrics request/response schemas (internal_metrics.clj):
  - Define schema for POST body containing:
    - tap_name, tap_version, mode (sync/check)
    - status (succeeded/failed)
    - duration_seconds (float)
  - Simple 200 OK response on success
3. Add metrics ingestion route (v1.clj):
  - Route: POST /v1/internal/metrics/extraction-job
  - Authentication: auth/validate-token (refresh token from orchestrator)
  - Handler increments counter and observes histogram duration
  - Returns {:status 200}")))
    
    (it "handles simple numbered list with nested bullets"
      (let ((input "# Item 1
# Item 2
# Item 3 with sub-items:
#* Sub-item A
#* Sub-item B
# Item 4
# Item 5"))
        (expect (go-jira-markup-to-org input)
                :to-equal "1. Item 1
2. Item 2
3. Item 3 with sub-items:
  - Sub-item A
  - Sub-item B
4. Item 4
5. Item 5"))))
  
  (describe "deeply nested lists"
    
    (it "converts three-level nested bulleted list correctly"
      (let ((input "* One
** Two
*** Three"))
        (expect (go-jira-markup-to-org input)
                :to-equal "- One
  - Two
    - Three")))
    
    (it "does not confuse three asterisks with org headings"
      (let ((input "* Item
** Sub-item
*** Sub-sub-item"))
        (let ((result (go-jira-markup-to-org input)))
          (expect result :to-match "^- Item$")
          (expect result :to-match "^  - Sub-item$")
          (expect result :to-match "^    - Sub-sub-item$")
          (expect result :not :to-match "\\*\\*\\*")))))
  
  (describe "basic inline formatting"
    
    (it "converts inline code"
      (expect (go-jira-markup-to-org "Some {{code}} here")
              :to-match "~code~"))
    
    (it "converts bold text"
      (expect (go-jira-markup-to-org "Some *bold* text")
              :to-equal "Some *bold* text"))
    
    (it "converts italic text"
      (expect (go-jira-markup-to-org "Some _italic_ text")
              :to-match "/italic/"))
    
    (it "converts strikethrough text"
      (expect (go-jira-markup-to-org "Some -strikethrough- text")
              :to-equal "Some +strikethrough+ text"))
    
    (it "converts underline/insert text"
      (expect (go-jira-markup-to-org "Some +underlined+ text")
              :to-equal "Some _underlined_ text")))
  
  (describe "headings"
    
    (it "converts h1 with jira-heading property"
      (let ((result (go-jira-markup-to-org "h1. Main heading")))
        (expect result :to-match "Main heading")
        (expect (get-text-property 0 'jira-heading result) :to-equal 1)))
    
    (it "converts h2 with jira-heading property"
      (let ((result (go-jira-markup-to-org "h2. Sub heading")))
        (expect result :to-match "Sub heading")
        (expect (get-text-property 0 'jira-heading result) :to-equal 2)))
    
    (it "converts h3 with jira-heading property"
      (let ((result (go-jira-markup-to-org "h3. Run linter")))
        (expect result :to-match "Run linter")
        (expect (get-text-property 0 'jira-heading result) :to-equal 3)))
    
    (it "converts h4-h6 with jira-heading properties"
      (let ((result4 (go-jira-markup-to-org "h4. Details"))
            (result5 (go-jira-markup-to-org "h5. More"))
            (result6 (go-jira-markup-to-org "h6. Even more")))
        (expect (get-text-property 0 'jira-heading result4) :to-equal 4)
        (expect (get-text-property 0 'jira-heading result5) :to-equal 5)
        (expect (get-text-property 0 'jira-heading result6) :to-equal 6))))
  
  (describe "code blocks"
    
    (it "converts code blocks with language"
      (let ((input "{code:java}
public class Test {}
{code}"))
        (let ((result (go-jira-markup-to-org input)))
          (expect result :to-match "#\\+begin_src java")
          (expect result :to-match "public class Test {}")
          (expect result :to-match "#\\+end_src"))))
    
    (it "converts code blocks without language"
      (let ((input "{code}
some code
{code}"))
        (let ((result (go-jira-markup-to-org input)))
          (expect result :to-match "#\\+begin_src")
          (expect result :to-match "some code"))))))

(provide 'go-jira-markup-tests)
;;; go-jira-markup-tests.el ends here
