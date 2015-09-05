function TreeNode(val, left, right) {
    this.val = val;
    this.left = left || null;
    this.right = right || null;
}

/**
 * @param {TreeNode} root
 * @return {TreeNode}
 */
var invertTree = function(root) {
    if (root === null) {
        return root;
    }

    swap(root);
    invertTree(root.left);
    invertTree(root.right);

    return root;
};

var swap = function (node) {
    var tmp = node.left;
    node.left = node.right;
    node.right = tmp;
};

var main = function () {
    var t1 = new TreeNode(4, new TreeNode(2), new TreeNode(7, new TreeNode(1), new TreeNode(3)));

    console.log(t1);

    console.log(invertTree(t1));
}

main();
