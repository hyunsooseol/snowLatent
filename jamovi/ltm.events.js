<!DOCTYPE html>
<html>
<head>
  <title>Latent Transition Analysis</title>
  <style>
    .model-formula-container {
      margin-bottom: 20px;
    }
    .model-formula-row {
      display: flex;
      align-items: center;
      margin-bottom: 10px;
    }
    .model-formula-input {
      flex-grow: 1;
      margin-right: 10px;
    }
  </style>
</head>
<body>
  <h1>Latent Transition Analysis</h1>

  <div class="model-formula-container">
    <div class="model-formula-row">
      <textarea class="model-formula-input" rows="3" placeholder="Enter model formula"></textarea>
      <button class="add-formula-btn">Add</button>
    </div>
  </div>

  <div class="results-container">
    <h2>Model Formulas</h2>
    <ul class="model-formula-list"></ul>
  </div>

  <script>
    const modelFormulaContainer = document.querySelector('.model-formula-container');
    const addFormulaBtn = document.querySelector('.add-formula-btn');
    const modelFormulaList = document.querySelector('.model-formula-list');

    const modelFormulas = [];

    addFormulaBtn.addEventListener('click', () => {
      const newFormulaRow = document.createElement('div');
      newFormulaRow.classList.add('model-formula-row');

      const newFormulaInput = document.createElement('textarea');
      newFormulaInput.classList.add('model-formula-input');
      newFormulaInput.rows = 3;
      newFormulaInput.placeholder = 'Enter model formula';

      const newRemoveBtn = document.createElement('button');
      newRemoveBtn.classList.add('remove-formula-btn');
      newRemoveBtn.textContent = 'Remove';

      newFormulaRow.appendChild(newFormulaInput);
      newFormulaRow.appendChild(newRemoveBtn);
      modelFormulaContainer.appendChild(newFormulaRow);

      newRemoveBtn.addEventListener('click', () => {
        modelFormulaContainer.removeChild(newFormulaRow);
        const index = modelFormulas.indexOf(newFormulaInput.value);
        if (index > -1) {
          modelFormulas.splice(index, 1);
        }
        updateModelFormulaList();
      });

      modelFormulas.push(newFormulaInput.value);
      updateModelFormulaList();
    });

    function updateModelFormulaList() {
      modelFormulaList.innerHTML = '';
      modelFormulas.forEach(formula => {
        const listItem = document.createElement('li');
        listItem.textContent = formula;
        modelFormulaList.appendChild(listItem);
      });
    }
  </script>
</body>
</html>