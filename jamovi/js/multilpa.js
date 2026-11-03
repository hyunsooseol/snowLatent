// jus 3 handlers do not inherit the legacy this.clone() utility.
// Keep reference-category synchronization independent of the Run action.
const copyArray = function(value) {
    return Array.isArray(value) ? JSON.parse(JSON.stringify(value)) : [];
};

const synchronize = function(ui, context) {
    if (context.workspace.multilpaSynchronizing)
        return;
    context.workspace.multilpaSynchronizing = true;
    try {
        for (const level of ['individual', 'group']) {
            const factorControl = ui[level + 'Factors'];
            const referenceControl = ui[level + 'RefLevels'];
            const variables = copyArray(factorControl.value());
            const current = copyArray(referenceControl.value());
            const next = variables.map(variable => {
                const previous = current.find(row => row && row.var === variable);
                return previous || { var: variable, ref: null };
            });
            if (JSON.stringify(current) !== JSON.stringify(next))
                referenceControl.setValue(next);
            const rows = referenceControl.value() || [];
            referenceControl.applyToItems(0, (item, index, column) => {
                if (column === 1 && rows[index])
                    item.setPropertyValue('variable', rows[index].var);
            });
        }
    }
    finally {
        context.workspace.multilpaSynchronizing = false;
    }
};

module.exports = {
    update: function(ui) { synchronize(ui, this); },
    view_loaded: function(ui) { synchronize(ui, this); },
    individualFactors_changed: function(ui) { synchronize(ui, this); },
    groupFactors_changed: function(ui) { synchronize(ui, this); },
    individualRefLevels_changed: function(ui) { synchronize(ui, this); },
    groupRefLevels_changed: function(ui) { synchronize(ui, this); }
};
