#' Question first experiments
#'
#' @format A data frame in tidy format with response accuracy and reaction
#'         time as primary dependent variables.
#' \describe{
#'   \item{subj_id}{Unique identifier for each subject.}
#'   \item{seed}{Trial generation randomization seed.}
#'   \item{initials}{ID for experimenter collecting the data.}
#'   \item{date}{Date and time data was collected.}
#'   \item{computer}{Testing computer by lab username.}
#'   \item{block}{Block of experiment. -1 for practice block.}
#'   \item{trial}{Trial number. n < 0 for trials in practice block.}
#'   \item{cue}{Name of cued object, e.g., "alligator".}
#'   \item{cue_file}{File name of recorded cue used on trial.}
#'   \item{mask_type}{Masking condition. "mask" or "nomask".}
#'   \item{feat_type}{The type of question being asked. "visual" or "nonvisual".}
#'   \item{correct_response}{What they should have said.}
#'   \item{question}{The question being asked, e.g., "Does it have a tail?"}
#'   \item{response}{Subject response. "yes" or "no".}
#'   \item{rt}{In milliseconds.}
#'   \item{is_correct}{1 or 0.}
#'   \item{exp_run}{Experiment run (original and replications).}
#'   \item{proposition_id}{Unique identifier for entire proposition used on this trial.}
#'   \item{block_type}{}
#'   \item{exp}{Labels this data as coming from a question_first experiment.}
#' }
#'
#' @name question_first
#' @docType data
NULL
