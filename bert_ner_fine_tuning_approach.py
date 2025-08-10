from transformers import AutoTokenizer, AutoModelForTokenClassification, Trainer, TrainingArguments, DataCollatorForTokenClassification
from datasets import Dataset, DatasetDict
from sklearn.model_selection import train_test_split
import json
import torch

# 1. Import bert-ner model and ler-template for finetuning
MODEL_NAME = "nlpaueb/bert-base-greek-uncased-v1"  # ή ό,τι tokenizer χρησιμοποιείς
DATASET_PATH = "/media/stathis/KINGSTON/ML-Water/filtered_data/final_classification_march_9/training_data/LER-TEMPLATE.MARCH"  # Το path του dataset

# === 2. Prepare tokenizer ===
tokenizer = AutoTokenizer.from_pretrained(MODEL_NAME)

# === 3. Import fine_tuning dataset ===
with open(DATASET_PATH, "r", encoding="utf-8") as f:
    data = json.load(f)

# === 4. Create label2id και id2label ===
all_tags = sorted({t["tag"] for ex in data for t in ex["tokens"]})
label2id = {label: idx for idx, label in enumerate(all_tags)}
id2label = {idx: label for label, idx in label2id.items()}

# === 5. Function for tokenization & alignment ===
def tokenize_and_align_labels(example):
    tokens = [t["token"] for t in example["tokens"]]
    tokenized = tokenizer(
        tokens,
        is_split_into_words=True,
        truncation=True,
        padding='max_length',
        max_length=128,
        return_offsets_mapping=True
    )
    labels = []
    word_ids = tokenized.word_ids()
    previous_word_idx = None
    for word_idx in word_ids:
        if word_idx is None:
            labels.append(-100)
        elif word_idx != previous_word_idx:
            labels.append(label2id[example["tokens"][word_idx]["tag"]])
        else:
            labels.append(-100)  # ή label2id["I-xxx"] αν θες να συνεχίζεις την ετικέτα
        previous_word_idx = word_idx
    tokenized["labels"] = labels
    del tokenized["offset_mapping"]
    return tokenized

# === 6. Data separation in training dataset και validation ===
train_data, val_data = train_test_split(data, test_size=0.1, random_state=42)

# === 7. Creating tokenized dataset ===
train_dataset = Dataset.from_list([tokenize_and_align_labels(ex) for ex in train_data])
val_dataset = Dataset.from_list([tokenize_and_align_labels(ex) for ex in val_data])

tokenized_dataset = DatasetDict({
    "train": train_dataset,
    "validation": val_dataset
})

# === 8. Import model ===
model = AutoModelForTokenClassification.from_pretrained(
    MODEL_NAME, 
    num_labels=len(label2id),
    id2label=id2label,
    label2id=label2id
)

# === 9. Create Data Collator ===
data_collator = DataCollatorForTokenClassification(tokenizer)

# === 10. Define Training Arguments ===
training_args = TrainingArguments(
    output_dir="./finetuned_ner",  # Διεύθυνση για την αποθήκευση του μοντέλου
    per_device_train_batch_size=8,
    per_device_eval_batch_size=8,
    num_train_epochs=3,
    logging_dir="./logs",  # Διεύθυνση για τα logs
    logging_steps=10,
    save_steps=10,
    evaluation_strategy="steps",  # Αν έχεις δεδομένα για αξιολόγηση
)

# === 11. Define Trainer ===
trainer = Trainer(
    model=model,
    args=training_args,
    train_dataset=tokenized_dataset['train'],
    eval_dataset=tokenized_dataset['validation'],
    tokenizer=tokenizer,
    data_collator=data_collator,
)

# === 12. Fine-tune model ===
trainer.train()

# === 13. Save fine-tuned===
trainer.save_model("./finetuned_ner")

